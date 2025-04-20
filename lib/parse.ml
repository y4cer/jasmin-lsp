open Core
open Lexing
open Jasmin

module P = Jasmin.Parser

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

let parse_file (fname : string) : result = 
  let module C = (val Jasmin.CoreArchFactory.core_arch_x86 ~use_lea:true ~use_set0:true !Jasmin.Glob_options.call_conv) in
  let module Arch = Jasmin.Arch_full.Arch_from_Core_arch (C) in
  Logs.debug (fun m -> m "filename: %s" fname);
  try (
    let _env, _pprog, ast = Compile.parse_file Arch.arch_info ~idirs:!Glob_options.idirs fname 
    in Ok ast
  )
  with
    | Annot.AnnotationError (loc, code) -> 
      Logs.debug (fun m -> m "annotation error %t" code);
      Error (loc, "annotation error")
      (* raise (End_of_file) *)
    | Pretyping.TyError (loc, code) ->
      Logs.debug (fun m -> m "typing error %a" Pretyping.pp_tyerror code);
      Error (loc, "typing error")
    | Syntax.ParseError (loc, msg) ->
        let msg =
          match msg with
          | None -> "unexpected token" (* default message *)
          | Some msg -> msg
        in
        Logs.debug (fun m -> m "parse error %s" msg);
        Error (loc, "parse error")

(* (* why didn't they annotate this? 
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
  () *)
