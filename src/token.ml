(** Token defintions *)

open Ast
open Type

(** A built-in keyword that has special meaning in parsing *)
type keyword =
  (* type def *)
  | KClass
  | KStruct
  | KInterface
  | KEnum
  | KExtends
  | KImplements
  | KImport
  | KPackage
  | KSuper
  (* type members *)
  | KVar
  | KVal
  | KFunc
  | KExtern
  | KNew
  (* modifiers *)
  | KStatic
  | KPublic
  | KPrivate
  | KVirtual
  | KOverride
  (* control flow *)
  | KIf
  | KElse
  | KWhile
  | KFor
  | KBreak
  | KContinue
  | KReturn
  (* special *)
  | KAs
  | KThis
  | KNull

(** Token definitions  *)
type token =
  | TKeyword of keyword
  | TIdent of string
  | TConst of const
  | TKPrim of primitive_ty
  | TBinOp of binop
  | TUnOp of unop
  | TDot
  | TComma
  | TColon
  | TOpenParen
  | TCloseParen
  | TOpenBracket
  | TCloseBracket
  | TOpenBrace
  | TCloseBrace
  | TAt
  | TSemicolon
  | TEof

let s_keyword = function
  (* type def *)
  | KClass -> "class"
  | KSuper -> "super"
  | KStruct -> "struct"
  | KInterface -> "interface"
  | KEnum -> "enum"
  | KExtends -> "extends"
  | KImplements -> "implements"
  | KImport -> "import"
  | KPackage -> "package"
  (* type members *)
  | KVar -> "var"
  | KVal -> "val"
  | KFunc -> "func"
  | KExtern -> "extern"
  | KNew -> "new"
  (* modifiers *)
  | KStatic -> "static"
  | KPublic -> "public"
  | KPrivate -> "private"
  | KVirtual -> "virtual"
  | KOverride -> "override"
  (* control flow *)
  | KIf -> "if"
  | KElse -> "else"
  | KWhile -> "while"
  | KFor -> "for"
  | KBreak -> "break"
  | KContinue -> "continue"
  | KReturn -> "return"
  (* special *)
  | KAs -> "as"
  | KThis -> "this"
  | KNull -> "null"

let s_token_def = function
  | TKeyword kw -> s_keyword kw
  | TIdent id -> id
  | TDot -> "."
  | TComma -> ","
  | TColon -> ":"
  | TOpenParen -> "("
  | TCloseParen -> ")"
  | TOpenBracket -> "["
  | TCloseBracket -> "]"
  | TOpenBrace -> "{"
  | TCloseBrace -> "}"
  | TConst c -> s_const c
  | TKPrim p -> s_primitive_ty p
  | TBinOp op -> s_binop op
  | TUnOp op -> s_unop op
  | TSemicolon -> ";"
  | TAt -> "@"
  | TEof -> "<end of file>"

let s_token (tk, _) = s_token_def tk
