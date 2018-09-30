open Ast
open Lex
open Parser
open Codegen
open Printf

(* register exception printers *)
let () =
  Printexc.register_printer (function
    | Lex.Error (kind, pos) ->
        Some (sprintf "%s: Lexer error: %s" (s_pos pos) (Lex.error_msg kind))
    | Typer.Error (kind, pos) ->
        Some (sprintf "%s: Typer error: %s" (s_pos pos) (Typer.error_msg kind))
    | Parser.Error (kind, pos) ->
        Some
          (sprintf "%s: Parser error: %s" (s_pos pos) (Parser.error_msg kind))
    | Codegen.Error (kind, pos) ->
        Some
          (sprintf "%s: Code generation error: %s" (s_pos pos) (Codegen.error_msg kind))
    | _ -> None (* for other exceptions *) )

let verbose = ref false

let output = ref "main"

let main_source = ref None

let _ =
  let speclist =
    [ ("-v", Arg.Set verbose, "Turns on verbose mode")
    ; ("-o", Arg.Set_string output, "Sets output executable")
    ; ( "-m"
      , Arg.String
          (fun s ->
            print_endline s ;
            main_source := Some s )
      , "Set main source file" ) ]
  in
  let usage_txt = "Moka is a programming language and compiler. Options:" in
  Arg.parse speclist print_endline usage_txt ;
  let gen = Codegen.init () in
  let typer = Typer.init () in
  let ch =
    match !main_source with
    | Some out when not (Sys.file_exists out) ->
        raise (Failure (Printf.sprintf "Main file not found: %s" out))
    | Some out -> out
    | _ -> raise (Failure "No main file given")
  in
  let stream = lex_stream ch in
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
          Codegen.build gen !output )
    with e ->
      let msg = Printexc.to_string e in
      let stack = Printexc.get_backtrace () in
      Printf.eprintf "error: %s%s\n" msg stack ;
      raise e
  in
  ()
