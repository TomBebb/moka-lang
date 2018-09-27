open Ast
open Type

type ty_expr_def =
  | TEConst of const
  | TEIdent of string
  | TEField of ty_expr * string
  | TEBinOp of binop * ty_expr * ty_expr
  | TEUnOp of unop * ty_expr
  | TEBlock of ty_expr list
  | TECall of ty_expr * ty_expr list
  | TEParen of ty_expr

and ty_expr_meta = 
  {
  edef: ty_expr_def;
  ety: ty;
  }

and ty_expr = ty_expr_meta span


type ty_member_kind = TMVar of ty * ty_expr option | TMFunc of arg list * ty * ty_expr

type ty_member_def = {tmname: string; tmkind: ty_member_kind; tmmods: MemberMods.t}


type ty_member = ty_member_def span

type ty_type_def_meta =
  {tepath: path; tekind: type_def_kind; temods: ClassMods.t; temembers: ty_member list}

type ty_type_def = type_def_meta span


type error_kind =
  | UnresolvedIdent of string
  | UnresolvedPath of path
  | CannotBinOp of binop * ty * ty
  | UnresolvedField of ty * string
  | CannotField of ty
  | UnresolvedFieldType of string

let error_msg = function
  | UnresolvedIdent s -> "Failed to resolve identifier '" ^ s ^"'"
  | UnresolvedPath p -> "Unresolved path '" ^ s_path p ^ "'"
  | CannotBinOp (op, a, b) -> "Cannot peform operation '" ^ s_binop op ^"' on types " ^ s_ty a ^ " and " ^ s_ty b
  | UnresolvedField (t, field) -> "Unresolved field '" ^ field ^ "' on type " ^ s_ty t
  | CannotField t -> "Type " ^ s_ty t ^ " has no fields"
  | UnresolvedFieldType name -> "Type of field '" ^ name ^ "' could not be resolved"

exception Error of error_kind span

type type_context =
  { tvars: (string, ty) Hashtbl.t Stack.t
  ; ttypedefs: (path, Ast.type_def) Hashtbl.t }

let enter_block ctx =
  Stack.push (Hashtbl.create 12) ctx.tvars

let leave_block ctx =
  Stack.pop ctx.tvars

let resolve_field ctx ty name pos =
  let path = match ty with
  | TPath p -> p
  | _ -> raise (Error (CannotField ty, pos))
  in
  let (field, _) = match Hashtbl.find_opt ctx.ttypedefs path with
      | Some def -> def
      | _ -> raise (Error (UnresolvedPath path, pos))
  in
  match List.find_opt (fun (mem, _) -> mem.mname = name) field.emembers with
    | Some mem -> mem
    | None -> raise (Error (UnresolvedField (ty, name), pos))

let ty_of tex =
  let (meta, _) = tex in
  meta.ety


let find_var ctx name =
  let res = ref None in
  Stack.iter (fun tbl -> match Hashtbl.find_opt tbl name with
  | Some v -> res := Some v
  | None -> ()) ctx.tvars;
  !res

let type_of_const = function
  | CInt _ -> TInt
  | CFloat _ -> TFloat
  | CString _ -> TShort
  | CBool _ -> TBool
  | CNull -> TVoid

let rec type_expr ctx ex =
  let type_of_member ctx (def, pos) = match def.mkind with
    | MVar (Some ty, _) -> ty
    | MVar (None, Some ex) -> ty_of (type_expr ctx ex)
    | MVar _ -> raise (Error (UnresolvedFieldType (def.mname), pos))
    | MFunc (args, ret, _) -> TFunc (List.map (fun arg -> arg.atype) args, ret) in
  let (edef, pos) = ex in
  let mk def ty = ({edef = def; ety = ty; }, {pfile = pos.pfile; pmin = pos.pmin; pmax = pos.pmax;}) in
  match edef with
  | EConst c -> mk (TEConst c) (TPrim (type_of_const c))
  | EIdent id -> (match find_var ctx id with
    | Some v -> mk (TEIdent id) v
    | None -> raise (Error (UnresolvedIdent id, pos)))
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
  | EBinOp (op, a, b) ->
    let a = type_expr ctx a in
    let b = type_expr ctx b in
    let a_ty = ty_of a in
    let b_ty = ty_of b in
    let res_ty = match op with
    | OpAdd | OpSub | OpDiv | OpMul ->
      if (is_numeric a_ty) && (is_numeric b_ty) && (a_ty = b_ty) then
        Some a_ty
      else None
    | OpAssign -> if a_ty = b_ty then Some(a_ty) else None
    | OpEq -> if a_ty = b_ty then Some(TPrim TBool) else None
    in
    (match res_ty with
    | None -> raise (Error (CannotBinOp (op, a_ty, b_ty), pos))
    | Some ty -> mk (TEBinOp (op, a, b)) ty)
  | EBlock exs ->
    let exs = List.map (type_expr ctx) exs in
    mk (TEBlock exs) (TPrim TVoid)
  | ECall (func, args) ->
    let func = type_expr ctx func in
    let args = List.map (type_expr ctx) args in
    mk (TECall (func, args)) (TPrim TVoid)