open Ast
open Token
open Type

let digit = [%sedlex.regexp? '0' .. '9']

let int = [%sedlex.regexp? Plus digit]

let letter = [%sedlex.regexp? 'a' .. 'z']

let rec token buf =
  let mk tk =
    ( tk
    , { pmin= Sedlexing.lexeme_start buf
      ; pmax= Sedlexing.lexeme_end buf
      ; pfile= "???" } )
  in
  match%sedlex buf with
  | white_space -> token buf
  | int -> mk (TConst (CInt (int_of_string (Sedlexing.Utf8.lexeme buf))))
  | letter, Star ('A' .. 'Z' | 'a' .. 'z' | digit) ->
      mk (TIdent (Sedlexing.Utf8.lexeme buf))
  | '.' -> mk TDot
  | ',' -> mk TComma
  | ':' -> mk TColon
  | '(' -> mk TOpenParen
  | ')' -> mk TCloseParen
  | '[' -> mk TOpenBracket
  | ']' -> mk TCloseBracket
  | '{' -> mk TOpenBrace
  | '}' -> mk TCloseBrace
  | '+' -> mk (TBinOp OpAdd)
  | '-' -> mk (TBinOp OpSub)
  | '*' -> mk (TBinOp OpMul)
  | '/' -> mk (TBinOp OpDiv)
  | '!' -> mk (TUnOp OpNot)
  | "void" -> mk (TKPrim TVoid)
  | "bool" -> mk (TKPrim TBool)
  | "byte" -> mk (TKPrim TByte)
  | "short" -> mk (TKPrim TShort)
  | "int" -> mk (TKPrim TInt)
  | "long" -> mk (TKPrim TLong)
  | eof -> mk TEof
  | _ -> failwith "Unexpected character"

let lex_stream text =
  let buf = Sedlexing.Utf8.from_string text in
  let was_eof = ref false in
  Stream.from (fun _ ->
      if !was_eof then None
      else
        let tk = token buf in
        (match tk with TEof, _ -> was_eof := true | _ -> ()) ;
        Some tk )
