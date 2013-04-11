open Lexer
open Parser

let test s =
  let lexbuf = Lexing.from_string s in
  Parser.parse Lexer.token lexbuf
