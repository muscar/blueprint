%{
  open Ast
%}

%token <int> NUMBER
%token <string> STRING
%token <string> ATOM
%token <string> VARIABLE

%token LARROW

%token LPAR RPAR
%token LBRACK RBRACK

%token PLUS MINUS MUL DIV

%token PIPE

%token EMARK EMARK2 QMARK QMARK2
%token COMMA PERIOD SEMICOLON COLON

%token SQUOTE DQUOTE

%token EOF

%start parse
%type <Ast.plan> parse

%%

parse: plan { $1 }

plan: triggering_event LARROW plan_body PERIOD { { triggering_event = $1;
												   body = $3 } }

triggering_event: event_type goal_type formula { { event_type = $1;
												   goal_type = $2;
												   formula = $3 } }

formula: ATOM LPAR term_seq RPAR               { ($1, $3) }

event_type:
   | PLUS  { Add }
   | MINUS { Del }
	   
goal_type:
   | EMARK { Achievement }
   | QMARK { Test }

plan_body:                                     { [] }
   | plan_action                               { [$1] }
   | plan_action SEMICOLON plan_body           { $1::$3 }

plan_action: plan_action_prefix formula        { ($1, $2) }

plan_action_prefix:
   | EMARK  { Call }
   | EMARK2 { AsyncCall }
   | QMARK  { MVarTake }
   | QMARK2 { MVarRead }
   | PLUS   { MVarPut }

term:
     NUMBER                     { TNumber $1 }
   | STRING                     { TString $1 }
   | ATOM                       { TAtom $1 }
   | VARIABLE                   { TVariable $1 }
   | ATOM LPAR term_seq RPAR    { TStructure ($1, $3) }
   | LBRACK list_literal_elements RBRACK { $2 }

term_seq:
     term                       { [$1] }
   | term COMMA term_seq        { $1::$3 }

list_literal_elements:                         { nil }
   | term                                      { TStructure ("cons", [$1; nil]) }
   | term COMMA list_literal_elements          { TStructure ("cons", [$1; $3]) }
   | term PIPE term                            { TStructure ("cons", [$1; $3]) }
