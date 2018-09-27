open Lex
open Parser
open Codegen

let _ =
  let gen = Codegen.init () in
  let typer = Typer.init () in
  print_endline "lexing" ;
  let stream =
    lex_stream
      "class HelloWorld { static func main(): int { if 3 == 2 {12} else {24} \
       } }"
  in
  let _ =
    try
      Some
        ( print_endline "Parsing" ;
          let ty_def = parse_type_def stream in
          print_endline "Typing" ;
          let typed = Typer.type_type_def typer ty_def in
          print_endline "Generating" ;
          let _ = Codegen.pre_gen_typedef gen typed in
          let _ = Codegen.gen_typedef gen typed in
          Llvm.dump_module gen.gen_mod )
    with Parser.Error (kind, _) ->
      print_endline ("Parser Error: " ^ Parser.error_msg kind) ;
      None
  in
  ()
