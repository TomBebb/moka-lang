open Ast
open Type
open Printf

type ty_expr_def =
  | TEThis
  | TEConst of const
  | TEIdent of string
  | TEField of ty_expr * string
  | TEBinOp of binop * ty_expr * ty_expr
  | TEUnOp of unop * ty_expr
  | TEBlock of ty_expr list
  | TECall of ty_expr * ty_expr list
  | TEParen of ty_expr
  | TEIf of ty_expr * ty_expr * ty_expr option
  | TEWhile of ty_expr * ty_expr
  | TEVar of ty option * string * ty_expr
  | TENew of path * ty_expr list

and ty_expr_meta = {edef: ty_expr_def; ety: ty}

and ty_expr = ty_expr_meta span

type ty_member_kind =
  | TMVar of ty * ty_expr option
  | TMFunc of param list * ty * ty_expr
  | TMConstr of param list * ty_expr

type ty_member_def =
  { tmname: string
  ; tmkind: ty_member_kind
  ; tmmods: MemberMods.t
  ; tmty: ty
  ; tmatts: (string, const) Hashtbl.t }

type ty_member = ty_member_def span

type ty_type_def_meta =
  { tepath: path
  ; tekind: type_def_kind
  ; temods: ClassMods.t
  ; temembers: ty_member list }

type ty_type_def = type_def_meta span

type error_kind =
  | UnresolvedIdent of string
  | UnresolvedPath of path
  | CannotBinOp of binop * ty * ty
  | UnresolvedField of ty * string
  | CannotField of ty
  | UnresolvedFieldType of string
  | UnresolvedThis
  | Expected of ty
  | CannotCall of ty

let error_msg = function
  | UnresolvedIdent s -> sprintf "Failed to resolve identifier '%s'" s
  | UnresolvedPath p -> sprintf "Unresolved path '%s'" (s_path p)
  | UnresolvedThis -> "Unresolved this"
  | CannotBinOp (op, a, b) ->
      sprintf "Operation '%s' cannot be performed on types %s an %s"
        (s_binop op) (s_ty a) (s_ty b)
  | UnresolvedField (t, field) ->
      sprintf "Type %s has no field '%s'" (s_ty t) field
  | CannotField t ->
      sprintf "Type %s is not a struct or class, so has no fields" (s_ty t)
  | UnresolvedFieldType name ->
      sprintf "Type of field '%s' could not be resolved" name
  | Expected t -> sprintf "Expected type %s" (s_ty t)
  | CannotCall t -> sprintf "The type %s cannot be called" (s_ty t)

exception Error of error_kind span

type type_context =
  { tvars: (string, ty) Hashtbl.t Stack.t
  ; ttypedefs: (path, Ast.type_def) Hashtbl.t
  ; mutable tthis: path option
  ; mutable tin_static: bool }

let init () =
  { tvars= Stack.create ()
  ; ttypedefs= Hashtbl.create 2
  ; tthis= None
  ; tin_static= true }

let enter_block ctx = Stack.push (Hashtbl.create 12) ctx.tvars

let leave_block ctx = Stack.pop ctx.tvars

let resolve_field ctx ty name pos =
  let path =
    match ty with TPath p -> p | _ -> raise (Error (CannotField ty, pos))
  in
  let field, _ =
    match Hashtbl.find_opt ctx.ttypedefs path with
    | Some def -> def
    | _ -> raise (Error (UnresolvedPath path, pos))
  in
  match List.find_opt (fun (mem, _) -> mem.mname = name) field.emembers with
  | Some mem -> mem
  | None -> raise (Error (UnresolvedField (ty, name), pos))

let ty_of tex =
  let meta, _ = tex in
  meta.ety

let set_var ctx name ty = Hashtbl.add (Stack.top ctx.tvars) name ty

let rec type_expr ctx ex =
  let edef, pos = ex in
  let mk def ty =
    ({edef= def; ety= ty}, {pfile= pos.pfile; pmin= pos.pmin; pmax= pos.pmax})
  in
  match edef with
  | EConst c -> mk (TEConst c) (TPrim (type_of_const c))
  | EThis ->
      mk TEThis
        ( match ctx.tthis with
        | Some t -> TPath t
        | None -> raise (Error (UnresolvedThis, pos)) )
  | EIdent id -> (
      let v = find_var ctx id in
      match v with
      | Some v -> mk (TEIdent id) v
      | None -> raise (Error (UnresolvedIdent id, pos)) )
  | EVar (t, name, v) ->
      let v = type_expr ctx v in
      mk (TEVar (t, name, v)) (TPrim TVoid)
  | EParen inner ->
      let inner = type_expr ctx inner in
      mk (TEParen inner) (ty_of inner)
  | EField (o, f) ->
      let obj = type_expr ctx o in
      let member = resolve_field ctx (ty_of obj) f pos in
      mk (TEField (obj, f)) (type_of_member ctx member)
  | EUnOp (op, v) ->
      let v = type_expr ctx v in
      mk (TEUnOp (op, v)) (ty_of v)
  | EBinOp (op, a, b) -> (
      let a = type_expr ctx a in
      let b = type_expr ctx b in
      let a_ty = ty_of a in
      let b_ty = ty_of b in
      let res_ty =
        match op with
        | OpAdd | OpSub | OpDiv | OpMul ->
            if is_numeric a_ty && is_numeric b_ty && a_ty = b_ty then Some a_ty
            else None
        | OpAssign -> if a_ty = b_ty then Some a_ty else None
        | OpEq -> if a_ty = b_ty then Some (TPrim TBool) else None
      in
      match res_ty with
      | None -> raise (Error (CannotBinOp (op, a_ty, b_ty), pos))
      | Some ty -> mk (TEBinOp (op, a, b)) ty )
  | EBlock exs ->
      let ty = ref (TPrim TVoid) in
      let exs =
        List.map
          (fun ex ->
            let ty_ex = type_expr ctx ex in
            let meta, _ = ty_ex in
            ty := meta.ety ;
            ty_ex )
          exs
      in
      mk (TEBlock exs) !ty
  | ECall (func, args) ->
      let func = type_expr ctx func in
      let args = List.map (type_expr ctx) args in
      mk
        (TECall (func, args))
        ( match ty_of func with
        | TFunc (_, ret) -> ret
        | t -> raise (Error (CannotCall t, pos)) )
  | EIf (cond, if_e, None) ->
      let cond = type_expr ctx cond in
      let if_e = type_expr ctx if_e in
      let _, pos = cond in
      if ty_of cond != TPrim TBool then
        raise (Error (Expected (TPrim TBool), pos)) ;
      mk (TEIf (cond, if_e, None)) (ty_of if_e)
  | EIf (cond, if_e, Some else_e) ->
      let cond = type_expr ctx cond in
      let if_e = type_expr ctx if_e in
      let else_e = type_expr ctx else_e in
      let _, pos = cond in
      if ty_of cond != TPrim TBool then
        raise (Error (Expected (TPrim TBool), pos)) ;
      mk (TEIf (cond, if_e, Some else_e)) (ty_of if_e)
  | EWhile (cond, body) ->
      let cond = type_expr ctx cond in
      let body = type_expr ctx body in
      if ty_of cond != TPrim TBool then
        raise (Error (Expected (TPrim TBool), pos)) ;
      mk (TEWhile (cond, body)) (TPrim TVoid)
  | ENew (path, args) ->
      let _ =
        match Hashtbl.find_opt ctx.ttypedefs path with
        | Some d -> d
        | None -> raise (Error (UnresolvedPath path, pos))
      in
      let args = List.map (type_expr ctx) args in
      mk (TENew (path, args)) (TPath path)

and type_of_member ctx (def, pos) =
  match def.mkind with
  | MVar (Some ty, _) -> ty
  | MVar (None, Some ex) -> ty_of (type_expr ctx ex)
  | MVar _ -> raise (Error (UnresolvedFieldType def.mname, pos))
  | MFunc (params, ret, _) ->
      TFunc (List.map (fun par -> par.ptype) params, ret)
  | MConstr (params, _) ->
      TFunc (List.map (fun par -> par.ptype) params, TPrim TVoid)

and find_var ctx name =
  let res = ref None in
  Stack.iter
    (fun tbl ->
      if !res != None then ()
      else
        match Hashtbl.find_opt tbl name with
        | Some v -> res := Some v
        | None -> () )
    ctx.tvars ;
  let this =
    match ctx.tthis with
    | Some t -> t
    | None -> raise (Failure "No this vlaue")
  in
  let def, _ = Hashtbl.find ctx.ttypedefs this in
  List.iter
    (fun (meta, pos) ->
      if meta.mname = name then res := Some (type_of_member ctx (meta, pos)) )
    def.emembers ;
  !res

and type_of_const = function
  | CInt _ -> TInt
  | CFloat _ -> TFloat
  | CString _ -> TShort
  | CBool _ -> TBool
  | CNull -> TVoid

let type_member ctx (def, pos) =
  ctx.tin_static <- MemberMods.mem MStatic def.mmods ;
  let kind =
    match def.mkind with
    | MVar (Some ty, None) -> TMVar (ty, None)
    | MVar (_, Some ex) ->
        let ex = type_expr ctx ex in
        TMVar (ty_of ex, Some ex)
    | MVar (None, None) -> raise (Error (UnresolvedFieldType def.mname, pos))
    | MFunc (params, ret, body) ->
        let _ = enter_block ctx in
        List.iter (fun par -> set_var ctx par.pname par.ptype) params ;
        let body = type_expr ctx body in
        let _ = leave_block ctx in
        TMFunc (params, ret, body)
    | MConstr (params, body) ->
        let _ = enter_block ctx in
        List.iter (fun par -> set_var ctx par.pname par.ptype) params ;
        let body = type_expr ctx body in
        let _ = leave_block ctx in
        TMConstr (params, body)
  in
  ( { tmkind= kind
    ; tmname= def.mname
    ; tmmods= def.mmods
    ; tmatts= def.matts
    ; tmty= type_of_member ctx (def, pos) }
  , pos )

let type_type_def ctx (def, pos) =
  ctx.tthis <- Some def.epath ;
  Hashtbl.add ctx.ttypedefs def.epath (def, pos) ;
  ( { tepath= def.epath
    ; tekind= def.ekind
    ; temods= def.emods
    ; temembers= List.map (type_member ctx) def.emembers }
  , pos )

let rec s_ty_expr tabs (meta, _) =
  match meta.edef with
  | TEThis -> "this"
  | TEConst c -> s_const c
  | TEIdent id -> id
  | TEField (o, f) -> s_ty_expr tabs o ^ "." ^ f
  | TEBinOp (op, a, b) -> s_ty_expr tabs a ^ s_binop op ^ s_ty_expr tabs b
  | TEUnOp (op, a) -> s_unop op ^ s_ty_expr tabs a
  | TEBlock exs ->
      "{"
      ^ String.concat ("\n" ^ tabs) (List.map (s_ty_expr (tabs ^ "\t")) exs)
      ^ "}"
  | TECall (f, exs) ->
      s_ty_expr tabs f ^ "("
      ^ String.concat "," (List.map (s_ty_expr tabs) exs)
      ^ ")"
  | TEParen ex -> "(" ^ s_ty_expr tabs ex ^ ")"
  | TEIf (cond, if_e, None) ->
      "if " ^ s_ty_expr tabs cond ^ " " ^ s_ty_expr tabs if_e
  | TEIf (cond, if_e, Some else_e) ->
      "if " ^ s_ty_expr tabs cond ^ " " ^ s_ty_expr tabs if_e ^ " else "
      ^ s_ty_expr tabs else_e
  | TEWhile (cond, body) ->
      "while " ^ s_ty_expr tabs cond ^ " " ^ s_ty_expr tabs body
  | TEVar (None, name, ex) ->
      Printf.sprintf "var %s = %s" name (s_ty_expr tabs ex)
  | TEVar (Some t, name, ex) ->
      Printf.sprintf "var %s: %s = %s" name (s_ty t) (s_ty_expr tabs ex)
  | TENew (path, args) ->
      Printf.sprintf "new %s(%s)" (s_path path)
        (String.concat "," (List.map (s_ty_expr tabs) args))
