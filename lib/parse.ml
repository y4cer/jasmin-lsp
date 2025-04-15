open Core
open Lexing

module L = MenhirLib.LexerUtil
module E = MenhirLib.ErrorReports

module P = Jasmin.Parser

module I = P.MenhirInterpreter

type cur_pos = 
  {
    loc_lnum: int;
    loc_cnum: int;
  }
  
type result = Ok of Lexer.S.pprogram | Error of cur_pos * string
let pos_of_lexbuf lexbuf : cur_pos =
  let pos = lexbuf.lex_curr_p in
    { 
      loc_lnum = pos.pos_lnum - 1;
      loc_cnum = (pos.pos_cnum - pos.pos_bol);
    }

(* TODO: incremental parsing *)

(* Prints the line number and character number where the error occurred.*)
(* for debug only *)
let print_error_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  sprintf "Line:%d Position:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_string (s : string) : result = 
  let lexbuf = Lexing.from_string s in
  let fallback_msg = "Parse error" in
  try Ok (P.module_ Lexer.main lexbuf) with
  | Jasmin.Syntax.ParseError (_, msg) -> (
    let cur_pos = pos_of_lexbuf lexbuf in
    match msg with
    | Some msg_ -> Error (cur_pos, msg_)
    | None -> Error (cur_pos, fallback_msg) 
  )
  | Parser.Error -> Error (pos_of_lexbuf lexbuf, fallback_msg)
