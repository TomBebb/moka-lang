open Lex
open Parser

let _ =
  let gen = Codegen.init () in
  let stream = lex_stream "class Poop { }" in
  let _ =
    try Some (parse_type_def stream) with Parser.Error (kind, _) ->
      print_endline ("Parser Error: " ^ Parser.error_msg kind) ;
      None
  in
  let _ = Codegen.uninit gen in
  ()
