open Core
open Lexing
open Jasmin
open Jasmin.Prog

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

let string_of_position (pos : Lexing.position) =
  Printf.sprintf "%d:%d" pos.pos_lnum pos.pos_cnum

(* TODO: incremental parsing *)

(* Prints the line number and character number where the error occurred.*)
(* for debug only *)
let print_error_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  sprintf "Line:%d Position:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let iter_pprog (pprog_item : ('info, 'asm) pmod_item) = 
  let _  = match pprog_item with
(* type ('len,'info,'asm) gmod_item =
  | MIfun   of ('len,'info,'asm) gfunc
  | MIparam of ('len gvar * 'len gexpr)
  | MIglobal of ('len gvar * 'len ggexpr) *)
  | _ -> () 
  in 
  (* Logs.debug (fun m -> m "%s" @@ string_of_position pprog_item); *)
  ()


let parse_file (fname : string) : result = 
  let module C = (val Jasmin.CoreArchFactory.core_arch_x86 ~use_lea:true ~use_set0:true !Jasmin.Glob_options.call_conv) in
  let module Arch = Jasmin.Arch_full.Arch_from_Core_arch (C) in
  Logs.debug (fun m -> m "filename: %s" fname);
  try (
    let _env, pprog, ast = Compile.parse_file Arch.arch_info ~idirs:!Glob_options.idirs fname in
    let _ = List.iter ~f:iter_pprog pprog in
    Ok ast
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