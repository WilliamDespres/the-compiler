{
open Ast
open Parse
open Lexing
exception Eof
exception SyntaxError of string

(* gere les positions numero de ligne + decalage dans la ligne *)
let next_line lexbuf = Lexing.new_line lexbuf

let buf_pos_to_string lexbuf =
  let pos = lexbuf.lex_curr_p in
  pos.pos_fname ^ ":" ^ (string_of_int pos.pos_lnum) ^ ":" ^ (string_of_int (pos.pos_cnum - pos.pos_bol + 1))

let keyword_table = Hashtbl.create 13
  let _ =
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
      [
        "if", IF;
        "then", THEN;
        "else", ELSE;
        "is", IS;
        "var", VAR;
        "extends", EXTENDS;
        "def", DEF;
        "override", OVERRIDE;
        "new", NEW;
        "as", AS;
        "object", OBJECT;
        "class", CLASS;
        "return", RETURN
      ]

  let print_position lexbuf =
    let pos = lexbuf.lex_curr_p in
    Printf.printf "%s:%d:%d" pos.pos_fname
      pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
}

let whitespace = [' ''\t''\r']
let newline = ['\r''\n']
let lowercase = ['a'-'z' '_'] 
let uppercase = ['A'-'Z']
let letter = (lowercase | uppercase)

let digit = ['0'-'9']

let ld = (letter | digit)

rule comment = parse
  "*/"                { token lexbuf }
  | "\n"              { next_line lexbuf; comment lexbuf }
  | _                 { comment lexbuf }

and token = parse
  | lowercase ld* as id
                      { try Hashtbl.find keyword_table id with Not_found -> ID id }
  | uppercase ld* as id 
                      { IDCLASS id }
  | "/*"              { comment lexbuf }
  | whitespace        { token lexbuf }     (* skip blanks *)
  | newline           { next_line lexbuf; token lexbuf }
  | '+'               { PLUS }
  | '-'               { MINUS }
  | '/'               { DIV }
  | '*'               { TIMES }
  | '&'               { CONCAT }
  | ":="              { ASSIGN }
  | '('               { LPAREN }
  | ')'               { RPAREN }
  | '{'               { LBRACE }
  | '}'               { RBRACE }
  | '='               { RELOP (Ast.Eq) }
  | "<>"              { RELOP (Ast.Neq) }
  | '<'               { RELOP (Ast.Lt) }
  | '>'               { RELOP (Ast.Gt) }
  | "<="              { RELOP (Ast.Le) }
  | ">="              { RELOP (Ast.Ge) }
  | '.'               { DOT }
  | ';'               { SEMICOLON }
  | ':'               { COLON }
  | ','               { COMMA }
  | digit+ as c       { CSTE (int_of_string c) }
  | '"'               { read_stringCste (Buffer.create 16) lexbuf }
  | eof               { EOF }
  | _ as lxm          { raise (SyntaxError ((buf_pos_to_string lexbuf) ^ ": Undefined character: " ^ (String.make 1 lxm) )) }

and read_stringCste buf = parse
  | '"'               { STRINGCSTE (Buffer.contents buf) }
  | '\\' '\\'         { Buffer.add_char buf '\\'; read_stringCste buf lexbuf }
  | '\\' 'n'          { Buffer.add_string buf "\\n"; read_stringCste buf lexbuf }
  | '\\' 'r'          { Buffer.add_string buf "\\r"; read_stringCste buf lexbuf }
  | '\\' 't'          { Buffer.add_string buf "\\t"; read_stringCste buf lexbuf }
  | '\\' '"'          { Buffer.add_string buf "\\\""; read_stringCste buf lexbuf }
  | newline           { raise (SyntaxError ( (buf_pos_to_string lexbuf) ^ ": Illegal string character: newline" )) }
  | [^ '"' '\\'] as c { Buffer.add_char buf c; read_stringCste buf lexbuf }
  | eof               { raise (SyntaxError ("String is not terminated")) }
  | _ as lxm          { raise (SyntaxError ( (buf_pos_to_string lexbuf) ^ ": Illegal string character: " ^ (String.make 1 lxm) )) }
