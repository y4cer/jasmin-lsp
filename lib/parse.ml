open Lsp
open Types
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

let string_of_pos (pos : Position.t) =
  Printf.sprintf "line: %d, char: %d" pos.line pos.character

(* why didn't they annotate this? 
  Lexer.L.located.t.pl_loc.(loc_start|loc_end) (line, col) *)
let is_pos_in_range (pos : Position.t) (pl_loc : Jasmin.Location.t) = 
  let open Jasmin.Location in
  Logs.debug (fun m -> m "%s" (string_of_pos pos));
  Logs.debug (fun m -> m "%s" (tostring pl_loc));
  pos.line >= fst pl_loc.loc_start &&
  pos.line <= fst pl_loc.loc_end &&
  (pos.line > fst pl_loc.loc_start || pos.character >= snd pl_loc.loc_start) &&
  (pos.line < fst pl_loc.loc_end || pos.character <= snd pl_loc.loc_end)

let find_node_at_pos (pos : Position.t) (item : Lexer.S.pitem Lexer.L.located) = 
  if is_pos_in_range pos item.pl_loc then
    match item.pl_desc with
    | Lexer.S.PFundef fundef -> 
      Logs.debug (fun m -> m "%s" fundef.pdf_name.pl_desc);
       None
    | _ -> None
  else None

(* Trying to find the AST node by the given pos *)
let node_of_pos (pos : Position.t) (ast : Lexer.S.pprogram) = 
  let real_pos : Position.t = {line = pos.line + 1; character = pos.character} in
  let _a = List.map ast ~f:(find_node_at_pos real_pos) in
  ()
