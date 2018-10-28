(** Basic lexer. Transforms source code into stream of tokens *)
open Ast
open Token
open Type
open Core_kernel

type error_kind = Unexpected of int | UnexpectedEof of string

let error_msg = function
  | Unexpected got ->
      "Unexpected '" ^ String.make 1 (char_of_int got) ^ "' while lexing"
  | UnexpectedEof lexing -> "Unexpected end of file while lexing " ^ lexing

exception Error of error_kind span

type cursor = {lfile: string; mutable lline: int; mutable lline_start: int}

let init_cursor file = {lfile= file; lline= 1; lline_start= 0}

let curr = ref (init_cursor "")

let digit = [%sedlex.regexp? '0' .. '9']

let int = [%sedlex.regexp? Plus digit]

let float = [%sedlex.regexp? Plus digit, '.', Plus digit]

let letter = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z']

let mk buf tk =
  let resolve_offset offset =
    if offset >= !curr.lline_start then
      {pline= !curr.lline; pcol= offset - !curr.lline_start + 1}
    else {pline= !curr.lline; pcol= 0}
  in
  ( tk
  , { pmin= resolve_offset (Sedlexing.lexeme_start buf)
    ; pmax= resolve_offset (Sedlexing.lexeme_end buf)
    ; pfile= !curr.lfile } )

let mk_err buf tk =
  let resolve_offset offset =
    if offset >= !curr.lline_start then
      {pline= !curr.lline; pcol= offset - !curr.lline_start + 1}
    else {pline= !curr.lline; pcol= 0}
  in
  ( tk
  , { pmin= resolve_offset (Sedlexing.lexeme_start buf)
    ; pmax= resolve_offset (Sedlexing.lexeme_end buf)
    ; pfile= !curr.lfile } )

let rec token buf =
  let mk : 'a -> 'a span = mk buf in
  match%sedlex buf with
  | '\n' ->
      !curr.lline <- !curr.lline + 1 ;
      !curr.lline_start <- Sedlexing.lexeme_end buf ;
      token buf
  | white_space -> token buf
  | "void" -> mk (TKPrim TVoid)
  | "bool" -> mk (TKPrim TBool)
  | "byte" -> mk (TKPrim TByte)
  | "short" -> mk (TKPrim TShort)
  | "int" -> mk (TKPrim TInt)
  | "long" -> mk (TKPrim TLong)
  | "float" -> mk (TKPrim TFloat)
  | "String" -> mk (TKPrim TString)
  | "class" -> mk (TKeyword KClass)
  | "super" -> mk (TKeyword KSuper)
  | "interface" -> mk (TKeyword KInterface)
  | "struct" -> mk (TKeyword KStruct)
  | "extern" -> mk (TKeyword KExtern)
  | "enum" -> mk (TKeyword KEnum)
  | "extends" -> mk (TKeyword KExtends)
  | "implements" -> mk (TKeyword KImplements)
  | "import" -> mk (TKeyword KImport)
  | "var" -> mk (TKeyword KVar)
  | "val" -> mk (TKeyword KVal)
  | "virtual" -> mk (TKeyword KVirtual)
  | "override" -> mk (TKeyword KOverride)
  | "as" -> mk (TKeyword KAs)
  | "package" -> mk (TKeyword KPackage)
  | "func" -> mk (TKeyword KFunc)
  | "new" -> mk (TKeyword KNew)
  | "static" -> mk (TKeyword KStatic)
  | "public" -> mk (TKeyword KPublic)
  | "private" -> mk (TKeyword KPrivate)
  | "if" -> mk (TKeyword KIf)
  | "else" -> mk (TKeyword KElse)
  | "while" -> mk (TKeyword KWhile)
  | "for" -> mk (TKeyword KFor)
  | "break" -> mk (TKeyword KBreak)
  | "continue" -> mk (TKeyword KContinue)
  | "return" -> mk (TKeyword KReturn)
  | "this" -> mk (TKeyword KThis)
  | "null" -> mk (TKeyword KNull)
  | "true" -> mk (TConst (CBool true))
  | "false" -> mk (TConst (CBool false))
  | int -> mk (TConst (CInt (int_of_string (Sedlexing.Utf8.lexeme buf))))
  | float -> mk (TConst (CFloat (float_of_string (Sedlexing.Utf8.lexeme buf))))
  | '.' -> mk TDot
  | ',' -> mk TComma
  | ':' -> mk TColon
  | '(' -> mk TOpenParen
  | ')' -> mk TCloseParen
  | '[' -> mk TOpenBracket
  | ']' -> mk TCloseBracket
  | '{' -> mk TOpenBrace
  | '}' -> mk TCloseBrace
  | ';' -> mk TSemicolon
  | '@' -> mk TAt
  | "+=" -> mk (TBinOp OpAddAssign)
  | "-=" -> mk (TBinOp OpSubAssign)
  | "*=" -> mk (TBinOp OpMulAssign)
  | "/=" -> mk (TBinOp OpDivAssign)
  | '+' -> mk (TBinOp OpAdd)
  | '-' -> mk (TBinOp OpSub)
  | '*' -> mk (TBinOp OpMul)
  | '/' -> mk (TBinOp OpDiv)
  | '<' -> mk (TBinOp OpLt)
  | '!' -> mk (TUnOp OpNot)
  | "==" -> mk (TBinOp OpEq)
  | "!=" -> mk (TBinOp OpNEq)
  | '=' -> mk (TBinOp OpAssign)
  | '"' ->
      let strbuf = Buffer.create 1 in
      mk (TConst (CString (string1 strbuf buf)))
  | letter, Star (letter | digit | '_') ->
      mk (TIdent (Sedlexing.Utf8.lexeme buf))
  | eof -> mk TEof
  | _ ->
      raise
        (Error
           (mk_err buf
              (Unexpected
                 (Sedlexing.lexeme_char buf (Sedlexing.lexeme_start buf)))))

and string1 strbuf buf =
  let mk = mk buf in
  match%sedlex buf with
  | '"' -> Buffer.contents strbuf
  | "\\r" ->
      Buffer.add_char strbuf '\r' ;
      string1 strbuf buf
  | "\\n" ->
      Buffer.add_char strbuf '\n' ;
      string1 strbuf buf
  | eof -> raise (Error (mk (UnexpectedEof "string")))
  | any ->
      Buffer.add_string strbuf (Sedlexing.Utf8.lexeme buf) ;
      string1 strbuf buf
  | _ -> raise (Failure "failed to lex string")

(** Read from the source file [file] a stream of tokens *)
let lex_stream file =
  curr := init_cursor file ;
  let ch = In_channel.create ~binary:false file in
  let buf = Sedlexing.Utf8.from_channel ch in
  let was_eof = ref false in
  Stream.from (fun _ ->
      if !was_eof then None
      else
        let tk = token buf in
        (match tk with TEof, _ -> was_eof := true | _ -> ()) ;
        Some tk )
