(** Abstract Syntax Tree representation of constants, values, types, and expressions *)

open Type
open Core_kernel
type const =
  | CInt of int
  | CFloat of float
  | CString of string
  | CBool of bool
  | CNull

type binop =
  | OpAdd
  | OpSub
  | OpMul
  | OpDiv
  | OpAssign
  | OpLt
  | OpLe
  | OpGt
  | OpGe
  | OpEq
  | OpNEq
  | OpAddAssign
  | OpSubAssign
  | OpMulAssign
  | OpDivAssign

type unop = OpNeg | OpNot

type pos_detail = {pline: int; pcol: int}

type pos = {pfile: string; pmin: pos_detail; pmax: pos_detail}

type 'a span = 'a * pos

let span_v (v, _) = v

type variability = Variable | Constant

type expr_def =
  | ECast of expr * ty
  | EThis
  | ESuper
  | EConst of const
  | EIdent of string
  | EField of expr * string
  | EArrayIndex of expr * expr
  | EBinOp of binop * expr * expr
  | EUnOp of unop * expr
  | EBlock of expr list
  | ECall of expr * expr list
  | EParen of expr
  | EIf of expr * expr * expr option
  | EWhile of expr * expr
  | EVar of variability * ty option * string * expr
  | ENew of path * expr list
  | EReturn of expr option
  | ETuple of expr list
  | EBreak
  | EContinue

and expr = expr_def span

type param = {pname: string; ptype: ty}

type member_kind =
  | MVar of variability * ty option * const option
  | MFunc of param list * ty * expr
  | MConstr of param list * expr

type member_mod =
  | MStatic
  | MPublic
  | MPrivate
  | MExtern
  | MVirtual
  | MOverride

type member_mods = member_mod Set.Poly.t

type member_def =
  { mname: string
  ; mkind: member_kind
  ; mmods: member_mods
  ; matts: (string, const) Hashtbl.t }

type member = member_def span

type class_mod = CPublic | CPrivate

type class_mods = class_mod Set.Poly.t

type class_def = {cextends: path option; cimplements: path list}

type type_def_kind = EClass of class_def | EStruct

type type_def_meta =
  {epath: path; ekind: type_def_kind; emods: class_mods; emembers: member list}

type type_def = type_def_meta span

type module_def = {mimports: path list; mdefs: type_def list; mpackage: pack}

let s_pos p = Printf.sprintf "%s: %d:%d" p.pfile p.pmin.pline p.pmin.pcol

let s_const = function
  | CInt i -> string_of_int i
  | CFloat f -> string_of_float f
  | CString s -> "\"" ^ s ^ "\""
  | CBool b -> string_of_bool b
  | CNull -> "null"

let s_binop = function
  | OpAdd -> "+"
  | OpSub -> "-"
  | OpMul -> "*"
  | OpDiv -> "/"
  | OpAssign -> "="
  | OpLt -> "<"
  | OpLe -> "<="
  | OpGt -> ">"
  | OpGe -> ">="
  | OpEq -> "=="
  | OpNEq -> "!="
  | OpAddAssign -> "+="
  | OpSubAssign -> "-="
  | OpMulAssign -> "*="
  | OpDivAssign -> "/="

let s_unop = function OpNeg -> "-" | OpNot -> "!"

let s_variability = function Variable -> "var" | Constant -> "val"

let is_assign = function
  | OpAssign | OpAddAssign | OpSubAssign | OpMulAssign | OpDivAssign -> true
  | _ -> false

let inner_assign = function
  | OpAddAssign -> Some OpAdd
  | OpSubAssign -> Some OpSub
  | OpMulAssign -> Some OpMul
  | OpDivAssign -> Some OpDiv
  | _ -> None

let rec s_expr tabs (def, _) =
  match def with
  | ECast (v, t) -> sprintf "%s as %s" (s_expr tabs v) (s_ty t)
  | ESuper -> "super"
  | EThis -> "this"
  | EConst c -> s_const c
  | EIdent id -> id
  | EField (o, f) -> sprintf "%s.%s" (s_expr tabs o) f
  | EArrayIndex (a, i) -> sprintf "%s[%s]" (s_expr tabs a) (s_expr tabs i)
  | EBinOp (op, a, b) ->
      sprintf "%s %s %s" (s_expr tabs a) (s_binop op) (s_expr tabs b)
  | EUnOp (op, a) -> sprintf "%s%s" (s_unop op) (s_expr tabs a)
  | EBlock exs ->
      let tabs = tabs ^ "\t" in
      sprintf "{\n%s%s\n%s}" tabs
        (String.concat ~sep:("\n" ^ tabs) (List.map ~f:(s_expr tabs) exs))
        tabs
  | ECall (f, exs) ->
      sprintf "%s(%s)" (s_expr tabs f)
        (String.concat ~sep:"," (List.map ~f:(s_expr tabs) exs))
  | EParen ex -> sprintf "(%s)" (s_expr tabs ex)
  | EIf (cond, if_e, None) ->
      sprintf "if %s %s" (s_expr tabs cond) (s_expr tabs if_e)
  | EIf (cond, if_e, Some else_e) ->
      sprintf "if %s %s else %s" (s_expr tabs cond) (s_expr tabs if_e)
        (s_expr tabs else_e)
  | EWhile (cond, body) ->
      sprintf "while %s %s" (s_expr tabs cond) (s_expr tabs body)
  | EVar (v, None, name, ex) ->
      sprintf "%s %s = %s" (s_variability v) name (s_expr tabs ex)
  | EVar (v, Some t, name, ex) ->
      sprintf "%s %s: %s = %s" (s_variability v) name (s_ty t) (s_expr tabs ex)
  | ENew (path, args) ->
      sprintf "new %s(%s)" (s_path path)
        (String.concat ~sep:", " (List.map ~f:(s_expr tabs) args))
  | ETuple mems ->
      sprintf "(%s)" (String.concat ~sep:", " (List.map ~f:(s_expr tabs) mems))
  | EBreak -> "break"
  | EContinue -> "continue"
  | EReturn None -> "return"
  | EReturn (Some v) -> sprintf "return %s" (s_expr tabs v)

let s_var = function Variable -> "var" | Constant -> "val"

let s_param p = sprintf "%s: %s" p.pname (s_ty p.ptype)

let s_member ((mem, _) : member) : string =
  match mem.mkind with
  | MVar (vr, ty, va) ->
      sprintf "%s %s %s %s" (s_var vr) mem.mname
        (match ty with Some t -> ":" ^ s_ty t | _ -> "")
        (match va with Some v -> " = " ^ s_const v | _ -> "")
  | MFunc (pars, ret, body) ->
      sprintf "func %s(%s): %s %s" mem.mname
        (String.concat ~sep:"," (List.map ~f:s_param pars))
        (s_ty ret) (s_expr "\t" body)
  | MConstr (pars, body) ->
      sprintf "func new(%s) %s"
        (String.concat ~sep:"," (List.map ~f:s_param pars))
        (s_expr "\t" body)

let s_type_def ((def, _) : type_def) : string =
  let before, after =
    match def.ekind with
    | EClass {cextends= None; _} -> ("class", "")
    | EClass {cextends= Some ext; _} -> ("class", " extends" ^ s_path ext)
    | EStruct -> ("struct", "")
  in
  sprintf "%s %s%s {\n%s\n}" before (s_path def.epath) after
    (String.concat ~sep:"\n\t" (List.map ~f:s_member def.emembers))

let s_module m =
  sprintf "package %s\n%s" (s_pack m.mpackage)
    (String.concat ~sep:"\n" (List.map ~f:s_type_def m.mdefs))
