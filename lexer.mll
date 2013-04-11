{
  open Parser
}

let digit = ['0'-'9']
let lowercase_letter = ['a'-'z']
let uppercase_letter = ['A'-'Z']
let letter = lowercase_letter | uppercase_letter
let atom = lowercase_letter (letter | digit)*
let var = uppercase_letter (letter | digit)*

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }
| digit+ as lxm        { NUMBER (int_of_string lxm) }
| var as lxm           { VARIABLE lxm }
| atom as lxm          { ATOM lxm }
| "<-"                 { LARROW }
| '('                  { LPAR }
| ')'                  { RPAR }
| '['                  { LBRACK }
| ']'                  { RBRACK }
| '+'                  { PLUS }
| '-'                  { MINUS }
| '*'                  { MUL }
| '/'                  { DIV }
| '|'                  { PIPE }
| '!'                  { EMARK }
| "!!"                 { EMARK2 }
| "?"                  { QMARK }
| "??"                 { QMARK2 }
| ','                  { COMMA }
| '.'                  { PERIOD }
| ';'                  { SEMICOLON }
| ':'                  { COLON }
| '\''                 { SQUOTE }
| '"'                  { DQUOTE }
| eof                  { EOF }
