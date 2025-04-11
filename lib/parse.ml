open Core
open Lexing

module L = MenhirLib.LexerUtil
module E = MenhirLib.ErrorReports

module P = Jasmin.Parser

module I = P.MenhirInterpreter


let print_position outx (lexbuf : Lexing.lexbuf) =
  let pos = lexbuf.lex_curr_p in
  Core.fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

(* Prints the line number and character number where the error occurred.*)
let print_error_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  sprintf "Line:%d Position:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_string (s : string) = 
  let lexbuf = Lexing.from_string s in
  try Ok (P.module_ Lexer.main lexbuf) with
  | Jasmin.Syntax.ParseError _ -> print_endline @@ print_error_position lexbuf; Error (Error.of_string @@ print_error_position lexbuf)
  | Parser.Error -> Error (Error.of_string @@ print_error_position lexbuf)
