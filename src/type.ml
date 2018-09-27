type path = string list * string

type primitive_ty =
  | TVoid
  | TBool
  | TByte
  | TShort
  | TInt
  | TLong
  | TFloat
  | TDouble

type ty = TPrim of primitive_ty | TPath of path | TFunc of ty list * ty

let s_primitive_ty = function
  | TVoid -> "void"
  | TBool -> "bool"
  | TByte -> "byte"
  | TShort -> "short"
  | TInt -> "int"
  | TLong -> "long"
  | TFloat -> "float"
  | TDouble -> "double"

let s_path (parts, name) = String.concat "." (parts @ [name])

let rec s_ty = function
  | TPrim p -> s_primitive_ty p
  | TPath p -> s_path p
  | TFunc (args, ret) ->
      "func " ^ String.concat ", " (List.map s_ty args) ^ s_ty ret

let is_numeric = function
  | TPrim (TByte | TShort | TInt | TLong | TFloat | TDouble) -> true
  | _ -> false
