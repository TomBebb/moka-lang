open Type

type const =
  | CInt of int
  | CFloat of float
  | CString of string
  | CBool of bool
  | CNull

type binop = OpAdd | OpSub | OpMul | OpDiv | OpAssign | OpEq

type unop = OpNeg | OpNot

type pos = {pfile: string; pmin: int; pmax: int}

type 'a span = 'a * pos

type expr_def =
  | EConst of const
  | EIdent of string
  | EField of expr * string
  | EBinOp of binop * expr * expr
  | EUnOp of unop * expr
  | EBlock of expr list
  | ECall of expr * expr list
  | EParen of expr

and expr = expr_def span

type arg = {aname: string; atype: ty}

type member_kind = MVar of ty option * expr option | MFunc of arg list * ty * expr

type member_mod = MStatic | MPublic | MPrivate

module MemberMods = Set.Make (struct
  let compare = Pervasives.compare

  type t = member_mod
end)

type member_def = {mname: string; mkind: member_kind; mmods: MemberMods.t}

type member = member_def span

type class_mod = CPublic | CPrivate

module ClassMods = Set.Make (struct
  let compare = Pervasives.compare

  type t = class_mod
end)

type class_def = {cextends: path option; cimplements: path list}

type type_def_kind = EClass of class_def | EStruct

type type_def_meta =
  {epath: path; ekind: type_def_kind; emods: ClassMods.t; emembers: member list}

type type_def = type_def_meta span

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
  | OpEq -> "=="

let s_unop = function OpNeg -> "-" | OpNot -> "!"

let rec s_expr tabs (def, _) =
  match def with
  | EConst c -> s_const c
  | EIdent id -> id
  | EField (o, f) -> s_expr tabs o ^ "." ^ f
  | EBinOp (op, a, b) -> s_expr tabs a ^ s_binop op ^ s_expr tabs b
  | EUnOp (op, a) -> s_unop op ^ s_expr tabs a
  | EBlock exs ->
      "{"
      ^ String.concat ("\n" ^ tabs) (List.map (s_expr (tabs ^ "\t")) exs)
      ^ "}"
  | ECall (f, exs) ->
      s_expr tabs f ^ "("
      ^ String.concat "," (List.map (s_expr tabs) exs)
      ^ ")"
  | EParen ex -> "(" ^ s_expr tabs ex ^ ")"
