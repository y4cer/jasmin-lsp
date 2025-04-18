open Lsp
open Types
open Core
open Lexing
open Typing

module L = MenhirLib.LexerUtil
module E = MenhirLib.ErrorReports

module P = Jasmin.Parser

module I = P.MenhirInterpreter

type error_position = 
  {
    start_p: Lexing.position;
    end_p: Lexing.position;
  }
  
type result = Ok of Lexer.S.pprogram | Error of Lexer.L.t * string
let pos_of_lexbuf lexbuf : Lexer.L.t =
  let start_p = Lexing.lexeme_start_p lexbuf in
  let end_p = Lexing.lexeme_end_p lexbuf in
  Lexer.L.make start_p end_p

(* TODO: incremental parsing *)

(* Prints the line number and character number where the error occurred.*)
(* for debug only *)
let print_error_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  sprintf "Line:%d Position:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_string (s : string) : result = 
  Logs.warn (fun m -> m "PARSING!");
  let fname = "/home/drovosek/dip/lsp/jasmin-lsp/jasmin-examples/example.jazz" in 
  type_program fname; 
  let lexbuf = Lexing.from_string s in
  let fallback_msg = "Parse error" in
  try Ok (P.module_ Lexer.main lexbuf) with
  | Jasmin.Syntax.ParseError (loc, msg) -> (
    Logs.debug (fun m -> m "Jasmin parse error");
    match msg with
    | Some msg_ -> Error (loc, msg_)
    | None -> Error (loc, fallback_msg) 
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

let analyze_file fname _workspace =
  let _architecture = Jasmin.Glob_options.target_arch in
  Printf.eprintf "Analyzing file %s\n" fname;
  (* let get_ast ~fname = Option.bind (PathMap.find_opt fname workspace.open_documents) ~f:(fun st -> DocumentManager.get_ast st) in  *)
  ()

(* let find_node_at_pos (_pos : Position.t) (_item : _ Lexer.L.located) = 
  None *)

(* Trying to find the AST node by the given pos *)
let node_of_pos (pos : Position.t) (_ast : Lexer.S.pprogram) = 
  let _real_pos : Position.t = {line = pos.line + 1; character = pos.character} in
  (* let _a = List.map ast ~f:(find_node_at_pos real_pos) in *)
  ()
