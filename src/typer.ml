open Ast
open Type

type type_context =
  { tvars: (string, ty) Hashtbl.t Stack.t
  ; ttypedefs: (path, Ast.type_def) Hashtbl.t }

let type_of_const = function
  | CInt _ -> TInt
  | CFloat _ -> TFloat
  | CString _ -> TShort
  | CBool _ -> TBool
  | CNull -> TVoid

let rec type_of ctx (ex, _) : expr =
  match ex with
  | EConst c -> type_of_const c
  | EIdent id -> Hashtbl.find ctx.tvars id
  | EField (v, f) -> type_of ctx v
  | EBinOp (op, a, b) ->
      let a = type_of ctx a in
      let b = type_of ctx b in
      a
