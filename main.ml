open Utils

open Lexer
open Parser
open Ir
open Emit

exception ParseError of exn * (int * int * string)

let parse lexbuf =
  try
    Parser.parse Lexer.token lexbuf
  with exn ->
    let tok = Lexing.lexeme lexbuf in
    let curr = lexbuf.Lexing.lex_curr_p in
    let line = curr.Lexing.pos_lnum in
    let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol - String.length tok + 1 in
    raise (ParseError (exn, (line + 1, cnum, tok)))

let with_file path f =
  let chan = open_in path in
  let res = f chan in
  close_in chan;
  res

let parse_file path = with_file path (parse >> Lexing.from_channel)

let parse_string = parse >> Lexing.from_string

let main () =
  parse_file Sys.argv.(1) |> ir_of_ast |> js_of_ir_object |> print_endline;;

main ()
