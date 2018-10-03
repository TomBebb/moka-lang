open Ast
open Lex
open Parser
open Codegen
open Core_kernel

(* register exception printers *)

let () =
  Caml.Printexc.register_printer (function
    | Lex.Error (kind, pos) ->
        Some (sprintf "%s: Lexer error: %s" (s_pos pos) (Lex.error_msg kind))
    | Typer.Error (kind, pos) ->
        Some (sprintf "%s: Typer error: %s" (s_pos pos) (Typer.error_msg kind))
    | Parser.Error (kind, pos) ->
        Some
          (sprintf "%s: Parser error: %s" (s_pos pos) (Parser.error_msg kind))
    | Codegen.Error (kind, pos) ->
        Some
          (sprintf "%s: Code generation error: %s" (s_pos pos)
             (Codegen.error_msg kind))
    | _ -> None (* for other exceptions *) )

let verbose = ref false

let output = ref "main"

let main_source = ref None

let opt_level = ref 0

let _ =
  let speclist =
    [ ("-v", Arg.Set verbose, "Turns on verbose mode")
    ; ("-o", Arg.Set_string output, "Sets output executable")
    ; ("-O", Arg.Set_int opt_level, "Set optimization level (0-3)")
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
          let mod_def = parse_mod stream in
          print_endline "Typing" ;
          let typed = Typer.type_mod typer mod_def in
          print_endline "Pre Generating" ;
          let _ = Codegen.pre_gen_mod gen typed in
          print_endline "Generating" ;
          let _ = Codegen.gen_mod gen typed in
          Llvm.dump_module gen.gen_mod ;
          Codegen.build gen !output !opt_level )
    with e ->
      Printf.eprintf "error: %s\n" (Exn.to_string e) ;
      raise e
  in
  ()
