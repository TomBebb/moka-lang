open Ast
open Type

type keyword =
  (* type def *)
  | KClass
  | KStruct
  | KInterface
  | KEnum
  | KExtends
  | KImplements
  | KImport
  (* type members *)
  | KVar
  | KVal
  | KFunc
  (* modifiers *)
  | KStatic
  | KPublic
  | KPrivate
  (* control flow *)
  | KIf
  | KElse
  | KWhile
  | KFor
  | KBreak
  | KContinue
  (* special *)
  | KThis
  | KNull

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
  | TEof

let s_keyword = function
  (* type def *)
  | KClass -> "class"
  | KStruct -> "struct"
  | KInterface -> "interface"
  | KEnum -> "enum"
  | KExtends -> "extends"
  | KImplements -> "implements"
  | KImport -> "import"
  (* type members *)
  | KVar -> "var"
  | KVal -> "val"
  | KFunc -> "func"
  (* modifiers *)
  | KStatic -> "static"
  | KPublic -> "public"
  | KPrivate -> "private"
  (* control flow *)
  | KIf -> "if"
  | KElse -> "else"
  | KWhile -> "while"
  | KFor -> "for"
  | KBreak -> "break"
  | KContinue -> "continue"
  (* special *)
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
  | TEof -> "<end of file>"

let s_token (tk, _) = s_token_def tk
