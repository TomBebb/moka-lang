open Llvm
open Llvm_target
open Type
open Typer
open Ast

type error_kind = UnresolvedLHS

let error_msg = function UnresolvedLHS -> "unresolved RHS"

exception Error of error_kind span

type gen_def_meta = {dstruct: lltype; dfield_indicies: (ty, int) Hashtbl.t}

type gen_ctx =
  { gen_ctx: llcontext
  ; gen_mod: llmodule
  ; gen_main: llvalue
  ; mutable gen_func: llvalue
  ; gen_builder: llbuilder
  ; gen_block: llbasicblock
  ; gen_vars: (string, llvalue) Hashtbl.t Stack.t
  ; gen_this: llvalue
  ; gen_local: lltype
  ; gen_typedefs: (path, gen_def_meta) Hashtbl.t }

let init () =
  Llvm_all_backends.initialize () ;
  let ctx = create_context () in
  let main_mod = create_module ctx "main" in
  let builder = builder ctx in
  let sig_ty = function_type (i32_type ctx) (Array.of_list []) in
  let main_func = declare_function "main" sig_ty main_mod in
  let entry = append_block ctx "entry" main_func in
  { gen_ctx= ctx
  ; gen_mod= main_mod
  ; gen_main= main_func
  ; gen_func= main_func
  ; gen_vars= Stack.create ()
  ; gen_this= Llvm.const_int (Llvm.i1_type ctx) 0
  ; gen_builder= builder
  ; gen_block= entry
  ; gen_local= Llvm.i1_type ctx
  ; gen_typedefs= Hashtbl.create 10 }

let enter_block ctx = Stack.push (Hashtbl.create 12) ctx.gen_vars

let leave_block ctx = Stack.pop ctx.gen_vars

let find_var ctx name =
  let res = ref None in
  Stack.iter
    (fun tbl ->
      match Hashtbl.find_opt tbl name with
      | Some v -> res := Some v
      | None -> () )
    ctx.gen_vars ;
  !res

let get err_msg = function None -> raise (Failure err_msg) | Some v -> v

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
    match find_var ctx id with
    | Some v -> v
    | None -> raise (Error (UnresolvedLHS, pos)) )
  | _ -> raise (Error (UnresolvedLHS, pos))

let rec gen_expr ctx (def, pos) =
  match def.edef with
  | TEConst c -> gen_const ctx c
  | TEIdent id -> (
    match find_var ctx id with
    | Some v -> Llvm.build_load v id ctx.gen_builder
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
      get "no expressions found" !last
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
  | _ -> raise (Failure (s_ty_expr "" (def, pos)))

let member_name _ path field = s_path path ^ "_" ^ field.tmname

let pre_gen_typedef ctx (meta, _) =
  let meta : ty_type_def_meta = meta in
  let types = Array.of_list [] in
  let push ty = types.(Array.length types) <- ty in
  List.iter
    (fun (field, _) ->
      let is_static = MemberMods.mem MStatic field.tmmods in
      let name = member_name ctx meta.tepath field in
      match field.tmkind with
      | TMVar (ty, _) ->
          let llty = gen_ty ctx ty in
          if is_static then
            let _ = Llvm.declare_global llty name ctx.gen_mod in
            ()
          else push llty
      | TMFunc ([], TPrim TInt, _) when is_static && field.tmname = "main" ->
          ()
      | TMFunc (args, ret, _) ->
          let llargs = ref (if is_static then [] else [ctx.gen_local]) in
          let args = List.map (fun a -> gen_ty ctx a.atype) args in
          List.iter (fun arg -> llargs := !llargs @ [arg]) args ;
          let sig_ty =
            Llvm.function_type (gen_ty ctx ret) (Array.of_list !llargs)
          in
          let _ = Llvm.declare_function name sig_ty ctx.gen_mod in
          () )
    meta.temembers ;
  match meta.tekind with
  | EClass _ | EStruct ->
      let gen = Llvm.named_struct_type ctx.gen_ctx (s_path meta.tepath) in
      struct_set_body gen types false

let gen_typedef ctx (meta, _) =
  let meta : ty_type_def_meta = meta in
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
      | TMFunc (args, ret, ex) ->
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
            (fun index arg ->
              let param = Llvm.param func index in
              let ptr =
                build_alloca (type_of param) arg.aname ctx.gen_builder
              in
              let _ = build_store param ptr ctx.gen_builder in
              set_var ctx arg.aname ptr )
            args ;
          let meta, _ = ex in
          let va = gen_expr ctx ex in
          ( if meta.ety = ret then
            let _ =
              if ret = TPrim TVoid then build_ret_void ctx.gen_builder
              else build_ret va ctx.gen_builder
            in
            () ) ;
          let _ = leave_block ctx in
          Llvm.position_at_end ctx.gen_block ctx.gen_builder ;
          ()
      | _ -> () )
    meta.temembers

let build ctx output_file =
  let triple = Target.default_triple () in
  let target = Target.by_triple triple in
  let target_mach = TargetMachine.create ~triple target in
  let object_file = output_file ^ ".o" in
  if Sys.file_exists object_file then
    raise (Falure "object file already exists!") ;
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
