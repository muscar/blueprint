{
  open Parser
}

let digit = ['0'-'9']
let lowercase_letter = ['a'-'z']
let uppercase_letter = ['A'-'Z']
let letter = lowercase_letter | uppercase_letter
let atom = lowercase_letter (letter | digit | '_')*
let var = uppercase_letter (letter | digit)*
let action = atom '.' (atom | '.')+

rule token = parse
  ['\r' '\n']          { Lexing.new_line lexbuf; token lexbuf }
| [' ' '\t']           { token lexbuf }
| digit+ as lxm        { NUMBER (int_of_string lxm) }
| var as lxm           { VARIABLE lxm }
| atom as lxm          { ATOM lxm }
| action as lxm        { ACTION lxm }
| "do"                 { DO }
| "\"" ([^ '"'])* "\"" as lxm  { STRING (String.sub lxm 1 (String.length lxm - 2)) }
| "<-"                 { LARROW }
| '('                  { LPAR }
| ')'                  { RPAR }
| '['                  { LBRACK }
| ']'                  { RBRACK }
| '+'                  { PLUS }
| '-'                  { MINUS }
| '*'                  { MUL }
| '/'                  { DIV }
| '&'                  { AMPERSAND }
| '|'                  { PIPE }
| '!'                  { EMARK }
| "!!"                 { EMARK2 }
| "?"                  { QMARK }
| "??"                 { QMARK2 }
| ','                  { COMMA }
| '.'                  { PERIOD }
| ';'                  { SEMICOLON }
| ':'                  { COLON }
| eof                  { EOF }
