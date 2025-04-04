module L = MenhirLib.LexerUtil
module E = MenhirLib.ErrorReports

module P = Jasmin.Parser

module I = P.MenhirInterpreter

let in_channel_of_string string =
  let (in_file_descr, out_file_descr) = Unix.pipe () in
  let in_channel = Unix.in_channel_of_descr in_file_descr in
  let out_channel = Unix.out_channel_of_descr out_file_descr in
  begin
    output_string out_channel string;
    flush out_channel;
    in_channel;
  end

let print_position outx (lexbuf : Lexing.lexbuf) =
  let pos = lexbuf.lex_curr_p in
  Core.fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error _lexbuf =
()

let parse_and_print _lexbuf = 
  ()

let parse_string (s : string) = 
  let lexbuf = Lexing.from_channel (in_channel_of_string s) in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "tmp" };
  parse_and_print lexbuf;
  ()