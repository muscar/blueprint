%{
  open Ast
%}

%token <int> NUMBER
%token <string> STRING
%token <string> ATOM
%token <string> VARIABLE
%token <string> ACTION

%token DO

%token LARROW

%token LPAR RPAR
%token LBRACK RBRACK

%token PLUS MINUS MUL DIV
%token AMPERSAND

%token PIPE

%token EMARK EMARK2 QMARK QMARK2
%token COMMA PERIOD SEMICOLON COLON

%token SQUOTE DQUOTE

%token EOF

%type <Ast.toplevel_entry list> parse

%start parse

%%

parse: toplevel              { $1 }

toplevel:
   | toplevel_entry          { [$1] }
   | toplevel_entry toplevel { $1::$2 }

toplevel_entry:
   | formula annotations PERIOD { Belief ($1, $2) }
   | clause                     { Clause $1 }

clause: triggering_event annotations plan_context LARROW plan_body PERIOD { { triggering_event = $1;
																			  annotations = $2;
																			  context = $3;
																			  body = $5 } }

triggering_event: event_type goal_type formula { { event_type = $1;
												   goal_type = $2;
												   formula = $3 } }

formula: 
   | ATOM                                      { ($1, []) }
   | ATOM LPAR term_seq RPAR                   { ($1, $3) }

event_type:
   | PLUS  { Add }
   | MINUS { Del }
	   
goal_type:
   | EMARK { Achievement }
   | QMARK { Test }

plan_context:
   |                 { TAtom "true" }
   | COLON logic_exp { $2 }

plan_body:
   | plan_action                               { Action $1 }
   | plan_action SEMICOLON plan_body           { ActionSeq ($1, $3) }
   | DO plan_action SEMICOLON plan_body        { ActionDo ($2, $4) }

plan_action:
   | plan_action_prefix formula          { ($1, $2) }
   | ACTION LPAR term_seq RPAR           { (ActionCall, ($1, $3)) }

plan_action_prefix:
   | EMARK  { Call }
   | EMARK2 { AsyncCall }
   | QMARK  { MVarTake }
   | QMARK2 { MVarRead }
   | PLUS   { MVarPut }

term:
     NUMBER                              { TNumber $1 }
   | STRING                              { TString $1 }
   | ATOM                                { TAtom $1 }
   | VARIABLE                            { TVariable $1 }
   | ATOM LPAR term_seq RPAR             { TStructure ($1, $3) }
   | LBRACK list_literal_elements RBRACK { $2 }

term_seq:
     term                       { [$1] }
   | term COMMA term_seq        { $1::$3 }

list_literal_elements:                         { nil }
   | term                                      { TStructure ("$cons", [$1; nil]) }
   | term COMMA list_literal_elements          { TStructure ("$cons", [$1; $3]) }
   | term PIPE term                            { TStructure ("$cons", [$1; $3]) }

logic_exp:
   | term                     { $1 }
   | term AMPERSAND logic_exp { TStructure ("$and", [$1; $3]) }
   | term PIPE logic_exp      { TStructure ("$or", [$1; $3]) }

formula_seq:
   |                           { [] }
   | formula                   { [$1] }
   | formula COMMA formula_seq { $1::$3 }

annotations:
   |                           { [] }
   | LBRACK formula_seq RBRACK { $2 }
