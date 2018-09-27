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

type ty = TPrim of primitive_ty | TPath of path

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

let s_ty = function TPrim p -> s_primitive_ty p | TPath p -> s_path p
