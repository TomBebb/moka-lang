type pack = string list

type path = pack * string

type primitive_ty =
  | TVoid
  | TBool
  | TByte
  | TShort
  | TInt
  | TLong
  | TFloat
  | TDouble
  | TString

type ty =
  | TPrim of primitive_ty
  | TPath of path
  | TFunc of ty list * ty
  | TClass of path
  | TTuple of ty list

let s_primitive_ty = function
  | TVoid -> "void"
  | TBool -> "bool"
  | TByte -> "byte"
  | TShort -> "short"
  | TInt -> "int"
  | TLong -> "long"
  | TFloat -> "float"
  | TDouble -> "double"
  | TString -> "String"

let s_pack parts = String.concat "." parts

let s_path (parts, name) = String.concat "." (parts @ [name])

let rec s_ty = function
  | TPrim p -> s_primitive_ty p
  | TPath p -> s_path p
  | TFunc (args, ret) ->
      "func " ^ String.concat ", " (List.map s_ty args) ^ s_ty ret
  | TClass p -> Printf.sprintf "Class<%s>" (s_path p)
  | TTuple p -> "(" ^ String.concat "," (List.map s_ty p) ^ ")"

let is_numeric = function
  | TPrim (TByte | TShort | TInt | TLong | TFloat | TDouble) -> true
  | _ -> false

let is_real = function TPrim (TFloat | TDouble) -> true | _ -> false
