open Llvm
open Llvm_target
open Type
open Typer
open Ast

type error_kind =
  | UnresolvedLHS

let error_msg = function
  | UnresolvedLHS -> "unresolved LHS"

exception Error of error_kind span

type gen_def_meta =
  { dstruct: lltype
  ; dfield_indicies: (string, int) Hashtbl.t
  ; dstatics: (string, llvalue) Hashtbl.t }

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
  ; gen_typedefs: (path, gen_def_meta) Hashtbl.t }

let init () =
  Llvm_all_backends.initialize () ;
  let ctx = create_context () in
  let main_mod = create_module ctx "main" in
  let builder = builder ctx in
  let sig_ty = function_type (i32_type ctx) (Array.of_list []) in
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
  ; gen_typedefs= Hashtbl.create 10 }

let enter_block ctx = Stack.push (Hashtbl.create 12) ctx.gen_vars

let leave_block ctx = Stack.pop ctx.gen_vars

let get err_msg = function None -> raise (Failure err_msg) | Some v -> v

let member_name _ path field =
  match Hashtbl.find_opt field.tmatts "LinkName" with
  | Some (CString name) when MemberMods.mem MStatic field.tmmods -> name
  | _ -> s_path path ^ "_" ^ field.tmname

let find_var ctx name should_load =
  let res = ref None in
  let maybe_load v = if should_load then build_load v name ctx.gen_builder else v in
  Stack.iter
    (fun tbl ->
      match Hashtbl.find_opt tbl name with
      | Some v ->
          res :=
            Some (maybe_load v)
      | None -> () )
    ctx.gen_vars ;
  let local_path = get "no local path" ctx.gen_local_path in
  let local_meta = Hashtbl.find ctx.gen_typedefs local_path in
  ( match Hashtbl.find_opt local_meta.dstatics name with
  | Some s -> res := Some (maybe_load s)
  | None -> () ) ;
  (match Hashtbl.find_opt local_meta.dfield_indicies name with
    | None -> ()
  | Some ind -> res := Some(maybe_load (build_struct_gep (get "no this" ctx.gen_this) ind name ctx.gen_builder)));
  !res

let set_var ctx name value = Hashtbl.add (Stack.top ctx.gen_vars) name value

let rec gen_ty ctx ty =
  match ty with
  | TFunc (args, ret) ->
      Llvm.function_type (gen_ty ctx ret)
        (Array.of_list (List.map (gen_ty ctx) args))
  | TPrim TVoid -> Llvm.void_type ctx.gen_ctx
  | TPrim TBool -> Llvm.i1_type ctx.gen_ctx
  | TPrim TByte -> Llvm.i8_type ctx.gen_ctx
  | TPrim TShort -> Llvm.i16_type ctx.gen_ctx
  | TPrim TInt -> Llvm.i32_type ctx.gen_ctx
  | TPrim TLong -> Llvm.i64_type ctx.gen_ctx
  | TPrim TFloat -> Llvm.float_type ctx.gen_ctx
  | TPrim TDouble -> Llvm.double_type ctx.gen_ctx
  | TPath path ->
      Llvm.pointer_type (Hashtbl.find ctx.gen_typedefs path).dstruct

let gen_const ctx = function
  | CInt i -> Llvm.const_int (gen_ty ctx (TPrim TInt)) i
  | CFloat f -> Llvm.const_float (gen_ty ctx (TPrim TFloat)) f
  | CString s -> Llvm.const_string ctx.gen_ctx s
  | CBool b -> Llvm.const_int (gen_ty ctx (TPrim TBool)) (if b then 1 else 0)
  | CNull -> Llvm.const_null (void_type ctx.gen_ctx)

let gen_expr_lhs ctx (def, pos) =
  match def.edef with
  | TEIdent id -> (
    match find_var ctx id false with
    | Some v -> v
    | None -> raise (Error (UnresolvedLHS, pos)) )
  | _ -> raise (Error (UnresolvedLHS, pos))

let rec gen_expr ctx (def, pos) =
  match def.edef with
  | TEConst c -> gen_const ctx c
  | TEIdent id -> (
      let v = find_var ctx id true in
      match v with
      | Some v -> v
      | None -> raise (Typer.Error (UnresolvedIdent id, pos)) )
  | TEVar (_, name, v) ->
      let v = gen_expr ctx v in
      let ptr = Llvm.build_alloca (type_of v) name ctx.gen_builder in
      let _ = Llvm.build_store v ptr ctx.gen_builder in
      set_var ctx name ptr ; v
  | TEBinOp (OpEq, a, b) ->
      let a_val = gen_expr ctx a in
      let b_val = gen_expr ctx b in
      build_icmp Llvm.Icmp.Eq a_val b_val "eq" ctx.gen_builder
  | TEBinOp (OpAssign, a, b) ->
      let a_ref = gen_expr_lhs ctx a in
      let b_val = gen_expr ctx b in
      Llvm.build_store b_val a_ref ctx.gen_builder
  | TEBinOp (((OpAdd | OpSub | OpMul | OpDiv) as op), a, b) ->
      let a_val = gen_expr ctx a in
      let b_val = gen_expr ctx b in
      assert (Llvm.type_of a_val = gen_ty ctx (span_v a).ety) ;
      assert (Llvm.type_of b_val = gen_ty ctx (span_v b).ety) ;
      let ty = def.ety in
      ( match (op, is_real ty) with
      | OpAdd, false -> Llvm.build_add
      | OpAdd, true -> Llvm.build_fadd
      | OpSub, false -> Llvm.build_sub
      | OpSub, true -> Llvm.build_fsub
      | OpMul, false -> Llvm.build_mul
      | OpMul, true -> Llvm.build_fmul
      | OpDiv, false -> Llvm.build_sdiv
      | OpDiv, true -> Llvm.build_fdiv
      | _ -> raise (Typer.Error (UnresolvedIdent "aaaa", pos)) )
        a_val b_val "tmp_op" ctx.gen_builder
  | TEBlock exprs ->
      let last = ref None in
      List.iter (fun ex -> last := Some (gen_expr ctx ex)) exprs ;
      gen_const ctx (CBool false)
  | TEIf (cond, if_e, Some else_e) ->
      let cond = gen_expr ctx cond in
      let then_bl = append_block ctx.gen_ctx "then" ctx.gen_func in
      let else_bl = append_block ctx.gen_ctx "else" ctx.gen_func in
      let after_bl = append_block ctx.gen_ctx "after" ctx.gen_func in
      let ptr =
        build_alloca (gen_ty ctx (ty_of if_e)) "ifval" ctx.gen_builder
      in
      let _ = build_cond_br cond then_bl else_bl ctx.gen_builder in
      Llvm.position_at_end then_bl ctx.gen_builder ;
      let _ = build_store (gen_expr ctx if_e) ptr ctx.gen_builder in
      let _ = build_br after_bl ctx.gen_builder in
      Llvm.position_at_end else_bl ctx.gen_builder ;
      let _ = build_store (gen_expr ctx else_e) ptr ctx.gen_builder in
      let _ = build_br after_bl ctx.gen_builder in
      Llvm.position_at_end after_bl ctx.gen_builder ;
      build_load ptr "ifval" ctx.gen_builder
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
  | TECall (func, args) ->
      let func = gen_expr ctx func in
      let args = List.map (gen_expr ctx) args in
      build_call func (Array.of_list args) "func_res" ctx.gen_builder
  | TENew (path, args) ->
      let meta = Hashtbl.find ctx.gen_typedefs path in
      let constr = Hashtbl.find meta.dstatics "new" in
      let args = List.map (gen_expr ctx) args in
      build_call constr (Array.of_list args) "new" ctx.gen_builder
  | _ -> raise (Failure (s_ty_expr "" (def, pos)))

let pre_gen_typedef ctx (meta, _) =
  let meta : ty_type_def_meta = meta in
  let types = ref [] in
  let indices = Hashtbl.create 16 in
  let push ty = types := !types @ [ty] in
  let statics = Hashtbl.create 16 in
  ctx.gen_local_path <- Some meta.tepath ;
  (* generate fields *)
  List.iter
    (fun (field, _) ->
      let is_static = MemberMods.mem MStatic field.tmmods in
      match field.tmkind with
      | TMVar (ty, _) when not is_static ->
          let llty = gen_ty ctx ty in
          Hashtbl.add indices field.tmname (List.length !types) ;
          push llty
      | _ -> () )
    meta.temembers ;
  ( match meta.tekind with
  | EClass _ | EStruct ->
      let gen = named_struct_type ctx.gen_ctx (s_path meta.tepath) in
      struct_set_body gen (Array.of_list !types) false ;
      let dmeta =
        {dstruct= gen; dfield_indicies= indices; dstatics= statics}
      in
      ctx.gen_this_ty <- gen ;
      Hashtbl.add ctx.gen_typedefs meta.tepath dmeta ) ;
  List.iter
    (fun (field, _) ->
      let is_static = MemberMods.mem MStatic field.tmmods in
      let name = member_name ctx meta.tepath field in
      match field.tmkind with
      | TMVar (ty, _) when is_static ->
          let llty = gen_ty ctx ty in
          let global = Llvm.declare_global llty name ctx.gen_mod in
          Hashtbl.add statics field.tmname global
      | TMVar (_, _) -> ()
      | TMFunc ([], TPrim TInt, _) when is_static && field.tmname = "main" ->
          ()
      | TMFunc (params, ret, _) ->
          let llpars =
            ref
              ( if is_static then []
              else [gen_ty ctx (TPath (get "no path found" ctx.gen_local_path))]
              )
          in
          let params = List.map (fun param -> gen_ty ctx param.ptype) params in
          List.iter (fun param -> llpars := !llpars @ [param]) params ;
          let sig_ty =
            Llvm.function_type (gen_ty ctx ret) (Array.of_list !llpars)
          in
          let func = Llvm.declare_function name sig_ty ctx.gen_mod in
          Hashtbl.add statics field.tmname func
      | TMConstr (params, _) ->
          let params = List.map (fun param -> gen_ty ctx param.ptype) params in
          let sig_ty =
            Llvm.function_type ctx.gen_this_ty (Array.of_list params)
          in
          let func = Llvm.declare_function name sig_ty ctx.gen_mod in
          Hashtbl.add statics field.tmname func )
    meta.temembers

let gen_typedef ctx (meta, _) =
  let meta : ty_type_def_meta = meta in
  ctx.gen_local_path <- Some meta.tepath ;
  let meta_meta = Hashtbl.find ctx.gen_typedefs meta.tepath in
  ctx.gen_this_ty <- meta_meta.dstruct ;
  List.iter
    (fun (field, _) ->
      let is_static = MemberMods.mem MStatic field.tmmods in
      let name = member_name ctx meta.tepath field in
      match field.tmkind with
      | TMVar (_, Some va) when is_static ->
          let global =
            get "global not found" (lookup_global name ctx.gen_mod)
          in
          set_initializer global (gen_expr ctx va)
      | TMFunc (args, ret, ex) when not (MemberMods.mem MExtern field.tmmods)
        ->
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
          List.iteri
            (fun index par ->
              let param = Llvm.param func index in
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
          ignore (leave_block ctx)
      | TMConstr (args, body) ->
          let func =
            get "constructor not found" (lookup_function name ctx.gen_mod)
          in
          let entry = append_block ctx.gen_ctx "entry" func in
          ctx.gen_func <- func ;
          position_at_end entry ctx.gen_builder ;
          enter_block ctx ;
          let this_ptr =
            build_alloca ctx.gen_this_ty "this_ptr" ctx.gen_builder
          in
          ctx.gen_this <- Some this_ptr ;
          List.iteri
            (fun index par ->
              let param = Llvm.param func index in
              let ptr =
                build_alloca (type_of param) par.pname ctx.gen_builder
              in
              let _ = build_store param ptr ctx.gen_builder in
              set_var ctx par.pname ptr )
            args ;
          ignore (gen_expr ctx body) ;
          ignore (leave_block ctx) ;
          let this_val = build_load this_ptr "this" ctx.gen_builder in
          ignore (build_ret this_val ctx.gen_builder)
      | _ -> () )
    meta.temembers

let build ctx output_file =
  let triple = Target.default_triple () in
  let target = Target.by_triple triple in
  let target_mach = TargetMachine.create ~triple target in
  let object_file = output_file ^ ".o" in
  if Sys.file_exists object_file then
    raise (Failure "object file already exists!") ;
  TargetMachine.emit_to_file ctx.gen_mod CodeGenFileType.ObjectFile object_file
    target_mach ;
  if
    Sys.command (Printf.sprintf "gcc -o %s %s -lgc" output_file object_file)
    == 1
  then raise (Failure "failed to link") ;
  Sys.remove object_file

let uninit ctx =
  dispose_module ctx.gen_mod ;
  dispose_context ctx.gen_ctx
