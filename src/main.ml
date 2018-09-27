open Ast
open Lex
open Parser

let _ =
  print_endline (s_const (CInt 3)) ;
  let gen = Codegen.init () in
  let stream = lex_stream "(2)" in
  print_endline (s_expr (parse_expr stream)) ;
  let _ = Codegen.uninit gen in
  ()
