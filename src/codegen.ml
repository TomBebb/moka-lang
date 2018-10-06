open Llvm
open Llvm_target
open Type
open Typer
open Ast
open Core_kernel

type error_kind = UnresolvedLHS | UnresolvedIdent of string

let error_msg = function
  | UnresolvedLHS -> "unresolved LHS"
  | UnresolvedIdent id -> "unresolved '" ^ id ^ "'"

exception Error of error_kind span

type gen_def_meta =
  { dstruct: lltype
  ; dfield_indicies: (string, int) Hashtbl.t
  ; dstatics: (string, llvalue) Hashtbl.t
  ; dmethods: (string, llvalue) Hashtbl.t
  ; dsuper: (path * int) option
  ; dkind: type_def_kind
  ; dfield_defaults: (string, llvalue) Hashtbl.t }

type gen_loop_meta = {lafter: llbasicblock; lstart: llbasicblock}

type gen_ctx =
  { gen_ctx: llcontext
  ; gen_mod: llmodule
  ; gen_main: llvalue
  ; mutable gen_func: llvalue
  ; gen_builder: llbuilder
  ; gen_vars: (string, llvalue) Hashtbl.t Stack.t
  ; mutable gen_this: llvalue option
  ; mutable gen_this_ty: lltype
  ; mutable gen_local_path: path option
  ; gen_typedefs: (path, gen_def_meta) Hashtbl.Poly.t
  ; gen_size_t: lltype
  ; gen_loops: gen_loop_meta Stack.t }

let init () =
  Llvm_all_backends.initialize () ;
  let ctx = create_context () in
  let main_mod = create_module ctx "main" in
  let builder = builder ctx in
  let size_t_size = Ctypes.sizeof Ctypes.size_t in
  let size_t = integer_type ctx (size_t_size * 8) in
  let malloc_sig = function_type (pointer_type (i8_type ctx)) [|size_t|] in
  ignore (declare_function "GC_malloc" malloc_sig main_mod) ;
  let sig_ty = function_type (i32_type ctx) [||] in
  let main_func = declare_function "main" sig_ty main_mod in
  let entry = append_block ctx "entry" main_func in
  position_at_end entry builder ;
  { gen_ctx= ctx
  ; gen_mod= main_mod
  ; gen_main= main_func
  ; gen_func= main_func
  ; gen_vars= Stack.create ()
  ; gen_this= None
  ; gen_this_ty= void_type ctx
  ; gen_builder= builder
  ; gen_local_path= None
  ; gen_typedefs= Hashtbl.Poly.create ()
  ; gen_size_t= size_t
  ; gen_loops= Stack.create () }

let enter_block ctx = Stack.push ctx.gen_vars (String.Table.create ~size:4 ())

let leave_block ctx = Stack.pop ctx.gen_vars

let get err_msg = function None -> raise (Failure err_msg) | Some v -> v

let member_name _ path field =
  match Hashtbl.find field.tmatts "LinkName" with
  | Some (CString name) when Set.mem field.tmmods MStatic -> name
  | _ -> s_path path ^ "_" ^ field.tmname

let maybe_load ctx name v should_load =
  let is_func =
    classify_type (type_of v) = TypeKind.Pointer
    && classify_type (element_type (type_of v)) = TypeKind.Function
  in
  if should_load && not is_func then build_load v name ctx.gen_builder else v

let rec resolve_pointer ctx ptr =
  let ptr_ty = type_of ptr in
  assert (classify_type ptr_ty = TypeKind.Pointer) ;
  if classify_type (element_type ptr_ty) = TypeKind.Pointer then
    resolve_pointer ctx (build_load ptr "ptr" ctx.gen_builder)
  else ptr

let rec find_member ctx obj_path obj_ptr name =
  let ptr = resolve_pointer ctx obj_ptr in
  assert (classify_type (element_type (type_of ptr)) = Struct) ;
  let meta = Hashtbl.find_exn ctx.gen_typedefs obj_path in
  match Hashtbl.find meta.dfield_indicies name with
  | Some ind -> Some (build_struct_gep ptr ind name ctx.gen_builder)
  | _ -> (
    match meta.dsuper with
    | Some (s, ind) ->
        let super_ptr = build_struct_gep ptr ind "super" ctx.gen_builder in
        find_member ctx s super_ptr name
    | _ -> None )

let find_var ctx name should_load =
  let res = ref None in
  Stack.iter ctx.gen_vars ~f:(fun (tbl : (string, llvalue) Hashtbl.t) ->
      match Hashtbl.find tbl name with
      | Some v when !res = None ->
          res := Some (maybe_load ctx name v should_load)
      | _ -> () ) ;
  let local_path = get "no local path" ctx.gen_local_path in
  let local_meta = Hashtbl.find_exn ctx.gen_typedefs local_path in
  ( match Hashtbl.find local_meta.dstatics name with
  | Some s when !res = None -> res := Some (maybe_load ctx name s should_load)
  | _ -> () ) ;
  let this = get "no this" ctx.gen_this in
  ( if !res = None then
    let mem = find_member ctx local_path this name in
    res :=
      Some
        (maybe_load ctx name
           (get ("member '" ^ name ^ "' not found") mem)
           should_load) ) ;
  !res

let set_var ctx name value =
  let vars = Stack.top_exn ctx.gen_vars in
  ignore (Hashtbl.add vars ~key:name ~data:value)

let rec find_method ctx path obj_ptr name =
  let meta = Hashtbl.find_exn ctx.gen_typedefs path in
  let ptr = resolve_pointer ctx obj_ptr in
  match Hashtbl.find meta.dmethods name with
  | Some m -> (ptr, m)
  | None -> (
    match meta.dsuper with
    | Some (s, ind) ->
        let super = build_struct_gep ptr ind "super" ctx.gen_builder in
        find_method ctx s super name
    | _ -> raise (Failure "method not found") )

let tbl_find tbl key fail =
  match Hashtbl.find tbl key with Some v -> v | None -> raise (Failure fail)

let rec gen_ty ctx ty =
  match ty with
  | TFunc (args, ret) ->
      Llvm.function_type (gen_ty ctx ret)
        (Array.of_list (List.map args ~f:(gen_ty ctx)))
  | TPrim TVoid -> Llvm.void_type ctx.gen_ctx
  | TPrim TBool -> Llvm.i1_type ctx.gen_ctx
  | TPrim TByte -> Llvm.i8_type ctx.gen_ctx
  | TPrim TShort -> Llvm.i16_type ctx.gen_ctx
  | TPrim TInt -> Llvm.i32_type ctx.gen_ctx
  | TPrim TLong -> Llvm.i64_type ctx.gen_ctx
  | TPrim TFloat -> Llvm.float_type ctx.gen_ctx
  | TPrim TDouble -> Llvm.double_type ctx.gen_ctx
  | TPrim TString -> Llvm.pointer_type (i8_type ctx.gen_ctx)
  | TPath path ->
      Llvm.pointer_type (tbl_find ctx.gen_typedefs path (s_path path)).dstruct
  | TTuple mems ->
      struct_type ctx.gen_ctx (Array.of_list (List.map ~f:(gen_ty ctx) mems))
  | _ -> raise (Failure "This type cannot be generated")

let gen_const ctx = function
  | CInt i -> Llvm.const_int (gen_ty ctx (TPrim TInt)) i
  | CFloat f -> Llvm.const_float (gen_ty ctx (TPrim TFloat)) f
  | CString s ->
      let c = Llvm.build_global_stringptr (s ^ "\x00") "str" ctx.gen_builder in
      let instr =
        build_in_bounds_gep c
          (Array.of_list [const_int (i32_type ctx.gen_ctx) 0])
          "str" ctx.gen_builder
      in
      instr
  | CBool b -> Llvm.const_int (gen_ty ctx (TPrim TBool)) (if b then 1 else 0)
  | CNull -> Llvm.const_pointer_null (void_type ctx.gen_ctx)

let gc_malloc ctx ty name =
  let func =
    get "no gc func found" (lookup_function "GC_malloc" ctx.gen_mod)
  in
  let size = size_of ty in
  let ptr = build_call func (Array.of_list [size]) "ptr" ctx.gen_builder in
  build_pointercast ptr (pointer_type ty) name ctx.gen_builder

let rec gen_defaults ctx ptr meta =
  Hashtbl.iter_keys meta.dfield_defaults ~f:(fun name ->
      let con = Hashtbl.find_exn meta.dfield_defaults name in
      let ind = Hashtbl.find_exn meta.dfield_indicies name in
      let field_ptr = build_struct_gep ptr ind name ctx.gen_builder in
      ignore (build_store con field_ptr ctx.gen_builder) ) ;
  let _ =
    match meta.dsuper with
    | Some (path, ind) ->
        let super_ptr = build_struct_gep ptr ind "super" ctx.gen_builder in
        let super_meta = Hashtbl.find_exn ctx.gen_typedefs path in
        gen_defaults ctx super_ptr super_meta
    | None -> ()
  in
  ()

let rec gen_expr (ctx : gen_ctx) ((def, pos) : ty_expr) : llvalue =
  let rec gen_expr_lhs ctx (def, pos) =
    match def.edef with
    | TEThis -> get "unresolved this" ctx.gen_this
    | TEIdent id -> (
      match find_var ctx id false with
      | Some v -> v
      | None -> raise (Error (UnresolvedIdent id, pos)) )
    | TEField (obj, f) ->
        let path =
          match ty_of obj with
          | TPath p -> p
          | _ -> raise (Failure "only path can be indexed")
        in
        let obj = gen_expr_lhs ctx obj in
        get f (find_member ctx path obj f)
    | TEArrayIndex (obj, ind) -> (
        let obj_v = gen_expr_lhs ctx obj in
        let ind = gen_expr ctx ind in
        match ty_of obj with
        | TTuple _ ->
            build_gep obj_v
              [|gen_const ctx (CInt 0); ind|]
              "tuplemem" ctx.gen_builder
        | _ -> raise (Failure "unable to index") )
    | _ -> raise (Error (UnresolvedLHS, pos))
  in
  match def.edef with
  | TEConst c -> gen_const ctx c
  | TEThis -> get "unresolved this" ctx.gen_this
  | TESuper -> (
      let this = get "unresolved this" ctx.gen_this in
      let meta =
        Hashtbl.find_exn ctx.gen_typedefs (get "no path" ctx.gen_local_path)
      in
      match meta.dsuper with
      | Some (_, ind) -> build_struct_gep this ind "super" ctx.gen_builder
      | _ -> raise (Failure "no super found") )
  | TEIdent id -> (
      let v = find_var ctx id true in
      match v with
      | Some v -> v
      | None -> raise (Error (UnresolvedIdent id, pos)) )
  | TEField (_, f) ->
      let ptr = gen_expr_lhs ctx (def, pos) in
      build_load ptr f ctx.gen_builder
  | TEArrayIndex (_, _) ->
      let ptr = gen_expr_lhs ctx (def, pos) in
      build_load ptr "arrind" ctx.gen_builder
  | TEVar (_, _, name, v) ->
      let v_val : llvalue = gen_expr ctx v in
      let ptr = Llvm.build_alloca (type_of v_val) name ctx.gen_builder in
      let _ = Llvm.build_store v_val ptr ctx.gen_builder in
      set_var ctx name ptr ; v_val
  | TEUnOp (OpNeg, v) ->
      let v_val = gen_expr ctx v in
      let ty = def.ety in
      (if is_real ty then build_fneg else build_neg)
        v_val "neg" ctx.gen_builder
  | TEUnOp (OpNot, v) ->
      let v_val = gen_expr ctx v in
      build_not v_val "not" ctx.gen_builder
  | TEBinOp (OpEq, a, b) ->
      let a_val = gen_expr ctx a in
      let b_val = gen_expr ctx b in
      build_icmp Llvm.Icmp.Eq a_val b_val "eq" ctx.gen_builder
  | TEBinOp (OpNEq, a, b) ->
      let a_val = gen_expr ctx a in
      let b_val = gen_expr ctx b in
      build_icmp Llvm.Icmp.Ne a_val b_val "neq" ctx.gen_builder
  | TEBinOp (OpLt, a, b) ->
      let a_val = gen_expr ctx a in
      let b_val = gen_expr ctx b in
      build_icmp Llvm.Icmp.Slt a_val b_val "lt" ctx.gen_builder
  | TEBinOp (OpAssign, a, b) ->
      let a_ref = gen_expr_lhs ctx a in
      let b_val = gen_expr ctx b in
      Llvm.build_store b_val a_ref ctx.gen_builder
  | TEBinOp (op, a, b) ->
      let a_val = gen_expr ctx a in
      let b_val = gen_expr ctx b in
      assert (Llvm.type_of a_val = gen_ty ctx (span_v a).ety) ;
      assert (Llvm.type_of b_val = gen_ty ctx (span_v b).ety) ;
      let ty = def.ety in
      let op, assign =
        match inner_assign op with Some v -> (v, true) | None -> (op, false)
      in
      let res =
        ( match (op, is_real ty) with
        | OpAdd, false -> Llvm.build_add
        | OpAdd, true -> Llvm.build_fadd
        | OpSub, false -> Llvm.build_sub
        | OpSub, true -> Llvm.build_fsub
        | OpMul, false -> Llvm.build_mul
        | OpMul, true -> Llvm.build_fmul
        | OpDiv, false -> Llvm.build_sdiv
        | OpDiv, true -> Llvm.build_fdiv
        | _ -> raise (Failure ("op " ^ s_binop op ^ " unimplemented")) )
          a_val b_val "tmp_op" ctx.gen_builder
      in
      if assign then
        ignore (build_store res (gen_expr_lhs ctx a) ctx.gen_builder) ;
      res
  | TEBlock exprs -> (
      let last = ref None in
      List.iter ~f:(fun ex -> last := Some (gen_expr ctx ex)) exprs ;
      match !last with Some v -> v | None -> gen_const ctx (CInt 0) )
  | TEIf (cond, if_e, Some else_e) ->
      let cond = gen_expr ctx cond in
      let then_bl = append_block ctx.gen_ctx "then" ctx.gen_func in
      let else_bl = append_block ctx.gen_ctx "else" ctx.gen_func in
      let after_bl = append_block ctx.gen_ctx "after" ctx.gen_func in
      let _ = build_cond_br cond then_bl else_bl ctx.gen_builder in
      Llvm.position_at_end then_bl ctx.gen_builder ;
      let if_val = gen_expr ctx if_e in
      let new_then_bl = insertion_block ctx.gen_builder in
      let _ = build_br after_bl ctx.gen_builder in
      Llvm.position_at_end else_bl ctx.gen_builder ;
      let else_val = gen_expr ctx else_e in
      let new_else_bl = insertion_block ctx.gen_builder in
      let _ = build_br after_bl ctx.gen_builder in
      Llvm.position_at_end after_bl ctx.gen_builder ;
      if ty_of if_e = TPrim TVoid || ty_of else_e = TPrim TVoid then
        gen_const ctx (CInt 0)
      else
        build_phi
          [(if_val, new_then_bl); (else_val, new_else_bl)]
          "if" ctx.gen_builder
  | TEIf (cond, if_e, None) ->
      let cond = gen_expr ctx cond in
      let then_bl = append_block ctx.gen_ctx "then" ctx.gen_func in
      let after_bl = append_block ctx.gen_ctx "after" ctx.gen_func in
      let _ = build_cond_br cond then_bl after_bl ctx.gen_builder in
      Llvm.position_at_end then_bl ctx.gen_builder ;
      let _ = gen_expr ctx if_e in
      let _ = build_br after_bl ctx.gen_builder in
      Llvm.position_at_end after_bl ctx.gen_builder ;
      cond
  | TEWhile (cond, if_e) ->
      let cond_bl = append_block ctx.gen_ctx "cond" ctx.gen_func in
      let inner_bl = append_block ctx.gen_ctx "then" ctx.gen_func in
      let after_bl = append_block ctx.gen_ctx "after" ctx.gen_func in
      Stack.push ctx.gen_loops {lstart= cond_bl; lafter= after_bl} ;
      ignore (build_br cond_bl ctx.gen_builder) ;
      Llvm.position_at_end cond_bl ctx.gen_builder ;
      let cond = gen_expr ctx cond in
      ignore (build_cond_br cond inner_bl after_bl ctx.gen_builder) ;
      Llvm.position_at_end inner_bl ctx.gen_builder ;
      ignore (gen_expr ctx if_e) ;
      ignore (build_br cond_bl ctx.gen_builder) ;
      Llvm.position_at_end after_bl ctx.gen_builder ;
      ignore (Stack.pop ctx.gen_loops) ;
      cond
  | TECall (({edef= TESuper; _}, _), args) ->
      let args = List.map ~f:(gen_expr ctx) args in
      let this_path = get "no path found" ctx.gen_local_path in
      let meta = Hashtbl.find_exn ctx.gen_typedefs this_path in
      let super_path, index = get "no super" meta.dsuper in
      let super_meta = Hashtbl.find_exn ctx.gen_typedefs super_path in
      let func = Hashtbl.find_exn super_meta.dstatics "new" in
      let this = get "this" ctx.gen_this in
      let ptr = build_struct_gep this index "super" ctx.gen_builder in
      build_call func (Array.of_list ([ptr] @ args)) "" ctx.gen_builder
  | TECall ((def, pos), args) ->
      let args = ref (List.map ~f:(gen_expr ctx) args) in
      let func =
        match def.edef with
        | TEField (o, f) -> (
            let obj_t = ty_of o in
            match obj_t with
            | TClass path ->
                let meta = Hashtbl.find_exn ctx.gen_typedefs path in
                Hashtbl.find_exn meta.dstatics f
            | TPath path ->
                let obj = gen_expr_lhs ctx o in
                let ptr, meth = find_method ctx path obj f in
                args := [ptr] @ !args ;
                meth
            | _ -> raise (Failure (sprintf "no path for %s" (s_ty obj_t))) )
        | _ -> gen_expr ctx (def, pos)
      in
      let is_void =
        classify_type (return_type (element_type (type_of func)))
        = TypeKind.Void
      in
      build_call func (Array.of_list !args)
        (if is_void then "" else "func_res")
        ctx.gen_builder
  | TENew (path, args) ->
      let meta = Hashtbl.find_exn ctx.gen_typedefs path in
      let constr = Hashtbl.find_exn meta.dstatics "new" in
      let args = List.map ~f:(gen_expr ctx) args in
      let ptr =
        match meta.dkind with
        | EStruct ->
            build_alloca meta.dstruct ("struct_" ^ s_path path) ctx.gen_builder
        | EClass _ -> gc_malloc ctx meta.dstruct ("class_" ^ s_path path)
      in
      gen_defaults ctx ptr meta ;
      ignore
        (build_call constr (Array.of_list ([ptr] @ args)) "" ctx.gen_builder) ;
      ptr
  | TEParen inner -> gen_expr ctx inner
  | TETuple mems ->
      let mems = List.map ~f:(gen_expr ctx) mems in
      let ty = gen_ty ctx (ty_of (def, pos)) in
      let ptr = build_alloca ty "tuple" ctx.gen_builder in
      List.iteri mems ~f:(fun ind mem ->
          let mem_ptr =
            build_struct_gep ptr ind (string_of_int ind) ctx.gen_builder
          in
          ignore (build_store mem mem_ptr ctx.gen_builder) ) ;
      build_load ptr "tuple" ctx.gen_builder
  | TEBreak ->
      let loop = Stack.top_exn ctx.gen_loops in
      ignore (build_br loop.lafter ctx.gen_builder) ;
      gen_const ctx (CInt 0)
  | TEContinue ->
      let loop = Stack.top_exn ctx.gen_loops in
      ignore (build_br loop.lstart ctx.gen_builder) ;
      gen_const ctx (CInt 0)
  | TEReturn None -> build_ret_void ctx.gen_builder
  | TEReturn (Some v) ->
      let v = gen_expr ctx v in
      build_ret v ctx.gen_builder

let pre_gen_typedef ctx (meta, _) =
  let meta : ty_type_def_meta = meta in
  let types = ref [] in
  let indices = String.Table.create () in
  let statics = String.Table.create () in
  let methods = String.Table.create () in
  let defaults = String.Table.create () in
  ctx.gen_local_path <- Some meta.tepath ;
  (* generate fields *)
  List.iter
    ~f:(fun (field, _) ->
      let is_static = Set.mem field.tmmods MStatic in
      match field.tmkind with
      | TMVar (_, ty, def) when not is_static -> (
          let llty = gen_ty ctx ty in
          ignore
            (Hashtbl.add indices ~key:field.tmname ~data:(List.length !types)) ;
          ignore (types := !types @ [llty]) ;
          match def with
          | Some v ->
              ignore
                (Hashtbl.add defaults ~key:field.tmname ~data:(gen_const ctx v))
          | None -> () )
      | _ -> () )
    meta.temembers ;
  ignore
    ( match meta.tekind with
    | EClass cl ->
        let extends =
          match cl.cextends with
          | Some path ->
              let super = Hashtbl.find_exn ctx.gen_typedefs path in
              let super_ty = super.dstruct in
              types := !types @ [super_ty] ;
              Some (path, List.length !types - 1)
          | _ -> None
        in
        let gen = named_struct_type ctx.gen_ctx (s_path meta.tepath) in
        struct_set_body gen (List.to_array !types) false ;
        let dmeta =
          { dstruct= gen
          ; dfield_indicies= indices
          ; dstatics= statics
          ; dmethods= methods
          ; dsuper= extends
          ; dkind= meta.tekind
          ; dfield_defaults= defaults }
        in
        ctx.gen_this_ty <- gen ;
        Hashtbl.add ctx.gen_typedefs ~key:meta.tepath ~data:dmeta
    | EStruct ->
        let gen = named_struct_type ctx.gen_ctx (s_path meta.tepath) in
        struct_set_body gen (List.to_array !types) false ;
        let dmeta =
          { dstruct= gen
          ; dfield_indicies= indices
          ; dstatics= statics
          ; dmethods= methods
          ; dsuper= None
          ; dkind= meta.tekind
          ; dfield_defaults= defaults }
        in
        ctx.gen_this_ty <- gen ;
        Hashtbl.add ctx.gen_typedefs ~key:meta.tepath ~data:dmeta ) ;
  List.iter
    ~f:(fun (field, _) ->
      let is_static = Set.mem field.tmmods MStatic in
      let is_extern = Set.mem field.tmmods MExtern in
      let name = member_name ctx meta.tepath field in
      match field.tmkind with
      | TMVar (Constant, _, Some c) when is_static ->
          let v = gen_const ctx c in
          let global = Llvm.define_global name v ctx.gen_mod in
          set_unnamed_addr true global ;
          ignore (Hashtbl.add statics ~key:field.tmname ~data:global)
      | TMVar (_, ty, None) when is_static ->
          let llty = gen_ty ctx ty in
          let global = Llvm.declare_global llty name ctx.gen_mod in
          ignore (Hashtbl.add statics ~key:field.tmname ~data:global)
      | TMVar (_, _, _) -> ()
      | TMFunc ([], TPrim TInt, _) when is_static && field.tmname = "main" ->
          ()
      | TMFunc (params, ret, _) ->
          let llpars =
            ref
              ( if is_static then []
              else [gen_ty ctx (TPath (get "no path found" ctx.gen_local_path))]
              )
          in
          let callconv, is_var =
            match (is_extern, Hashtbl.find field.tmatts "CallConv") with
            | _, Some (CString "vararg") -> (CallConv.c, true)
            | _, Some (CString "cold") -> (CallConv.cold, false)
            | true, _ -> (CallConv.c, false)
            | _ -> (CallConv.fast, false)
          in
          let params =
            List.map ~f:(fun param -> gen_ty ctx param.ptype) params
          in
          List.iter ~f:(fun param -> llpars := !llpars @ [param]) params ;
          let sig_ty =
            (if is_var then var_arg_function_type else function_type)
              (gen_ty ctx ret) (Array.of_list !llpars)
          in
          let func = Llvm.declare_function name sig_ty ctx.gen_mod in
          if not is_extern then set_function_call_conv callconv func ;
          ignore
            (Hashtbl.add
               (if is_static then statics else methods)
               ~key:field.tmname ~data:func)
      | TMConstr (params, _) ->
          let this =
            gen_ty ctx (TPath (get "no path found" ctx.gen_local_path))
          in
          let params =
            List.map ~f:(fun param -> gen_ty ctx param.ptype) params
          in
          let sig_ty =
            Llvm.function_type (void_type ctx.gen_ctx)
              (Array.of_list ([this] @ params))
          in
          let func = Llvm.declare_function name sig_ty ctx.gen_mod in
          ignore (Hashtbl.add statics ~key:field.tmname ~data:func) )
    meta.temembers ;
  meta.temembers

let gen_typedef ctx (meta, _) =
  ctx.gen_local_path <- Some meta.tepath ;
  let meta_meta = Hashtbl.find_exn ctx.gen_typedefs meta.tepath in
  ctx.gen_this_ty <- meta_meta.dstruct ;
  List.iter
    ~f:(fun (field, _) ->
      let is_static = Set.mem field.tmmods MStatic in
      let name = member_name ctx meta.tepath field in
      match field.tmkind with
      | TMVar (Variable, _, Some c) when is_static ->
          let global =
            get "global not found" (lookup_global name ctx.gen_mod)
          in
          set_global_constant true global ;
          set_externally_initialized false global ;
          set_initializer global (gen_const ctx c)
      | TMFunc (args, ret, ex) when not (Set.mem field.tmmods MExtern) ->
          let is_main = is_static && field.tmname = "main" && args = [] in
          let func, entry =
            if is_main then (ctx.gen_main, Llvm.entry_block ctx.gen_main)
            else
              let func =
                get "function not found" (lookup_function name ctx.gen_mod)
              in
              (func, append_block ctx.gen_ctx "entry" func)
          in
          ctx.gen_func <- func ;
          Llvm.position_at_end entry ctx.gen_builder ;
          enter_block ctx ;
          let index_offset =
            if is_static then 0
            else (
              ctx.gen_this <- Some (param func 0) ;
              1 )
          in
          List.iteri
            ~f:(fun index par ->
              let param = Llvm.param func (index_offset + index) in
              let ptr =
                build_alloca (type_of param) par.pname ctx.gen_builder
              in
              let _ = build_store param ptr ctx.gen_builder in
              set_var ctx par.pname ptr )
            args ;
          let meta, _ = ex in
          let va = gen_expr ctx ex in
          ( if meta.ety = ret then
            let _ =
              if ret = TPrim TVoid then build_ret_void ctx.gen_builder
              else build_ret va ctx.gen_builder
            in
            () ) ;
          ignore (leave_block ctx) ;
          print_endline (string_of_llvalue func) ;
          print_endline (sprintf "validating %s" name) ;
          Llvm_analysis.assert_valid_function func
      | TMConstr (args, body) ->
          let func =
            get "constructor not found" (lookup_function name ctx.gen_mod)
          in
          let entry = append_block ctx.gen_ctx "entry" func in
          ctx.gen_func <- func ;
          position_at_end entry ctx.gen_builder ;
          enter_block ctx ;
          ctx.gen_this <- Some (Llvm.param func 0) ;
          List.iteri
            ~f:(fun index par ->
              let param = Llvm.param func (index + 1) in
              let ptr =
                build_alloca (type_of param) par.pname ctx.gen_builder
              in
              let _ = build_store param ptr ctx.gen_builder in
              set_var ctx par.pname ptr )
            args ;
          ignore (gen_expr ctx body) ;
          ignore (build_ret_void ctx.gen_builder) ;
          ignore (leave_block ctx)
      | _ -> () )
    meta.temembers ;
  ()

let pre_gen_mod ctx tm =
  List.iter ~f:(fun def -> ignore (pre_gen_typedef ctx def)) tm.tmdefs

let gen_mod ctx tm = List.iter ~f:(gen_typedef ctx) tm.tmdefs

let optimize ctx opt_level =
  print_endline "Optimizing" ;
  let pmb = Llvm_passmgr_builder.create () in
  Llvm_passmgr_builder.set_opt_level opt_level pmb ;
  Llvm_passmgr_builder.use_inliner_with_threshold opt_level pmb ;
  let pm = PassManager.create () in
  Llvm_passmgr_builder.populate_module_pass_manager pm pmb ;
  ignore (PassManager.run_module ctx.gen_mod pm)

let build ctx output_file opt_level =
  optimize ctx opt_level ;
  Llvm.dump_module ctx.gen_mod ;
  let triple = Target.default_triple () in
  let target = Target.by_triple triple in
  let target_mach = TargetMachine.create ~triple target in
  let object_file = output_file ^ ".o" in
  if Sys.file_exists object_file then
    raise (Failure "object file already exists!") ;
  TargetMachine.emit_to_file ctx.gen_mod CodeGenFileType.ObjectFile object_file
    target_mach ;
  if Sys.command (sprintf "clang -o %s %s -lgc" output_file object_file) = 1
  then raise (Failure "failed to link") ;
  Sys.remove object_file

let uninit ctx =
  dispose_module ctx.gen_mod ;
  dispose_context ctx.gen_ctx
