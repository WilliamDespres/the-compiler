open Ast
open Lexing
open Print
open Verifs
open Compil

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try (
    let ld, b = Parse.program Lex.token lexbuf in (* Starts syntaxic analyzer and returns the AST of the program *)
    print_program ld b; (* Prints the AST of the program *)
    let env = program_verif ld b in (* Makes all necessary contextual verifications *)
    let classesInfo = preprocess_classes_infos env in (* TODO: returns the storage size needed to compile *)
    compile_program b env classesInfo
  )
  with
    Parse.Error ->
    Printf.fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

let _ =
  let file = open_in Sys.argv.(1) in
  let lexbuf = Lexing.from_channel file in
  let _ = parse_with_error lexbuf
  in close_in file
