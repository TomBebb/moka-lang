open Lex
open Parser
open Codegen

let () =
  Printexc.register_printer (function
    | Typer.Error (kind, _) ->
        Some (Printf.sprintf "Typer error: %s" (Typer.error_msg kind))
    | Parser.Error (kind, _) ->
        Some (Printf.sprintf "Parser error: %s" (Parser.error_msg kind))
    | _ -> None (* for other exceptions *) )

let _ =
  let gen = Codegen.init () in
  let typer = Typer.init () in
  print_endline "lexing" ;
  let stream =
    lex_stream
      "struct HelloWorld { static func add(a: int, b: int): int { a + b } \
       static func main(): void { var res = 12 + 12 } }"
  in
  Printexc.record_backtrace true ;
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
          Llvm.dump_module gen.gen_mod ;
          Codegen.run gen )
    with e ->
      let msg = Printexc.to_string e in
      let stack = Printexc.get_backtrace () in
      Printf.eprintf "error: %s%s\n" msg stack ;
      raise e
  in
  ()
