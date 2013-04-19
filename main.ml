open Lexer
open Parser

let parse_file path =
  let chan = open_in path in
  let lexbuf = Lexing.from_channel chan in
  let ast = Parser.parse Lexer.token lexbuf in
  close_in chan;
  ast

let parse_string s =
  let lexbuf = Lexing.from_string s in
  Parser.parse Lexer.token lexbuf
