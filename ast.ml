open Common

(** Terms *)
type term =
  | TNumber of int
  | TString of string
  | TAtom of name
  | TVariable of name
  | TStructure of name * term list

(** Derived ctors for terms *)
let nil = TAtom "nil"

let list first rest = TStructure ("cons", [first; rest])

(** Plans *)
type clause =
	{ triggering_event: triggering_event;
	  annotations: formula list;
	  context: term;
	  body: plan_action list }
and triggering_event =
	{ event_type: event_type;
	  goal_type: goal_type;
	  formula: formula }
and event_type = Add | Del
and goal_type = Achievement | Test
and plan_action = action_op * formula
and action_op = 
   | Call
   | AsyncCall
   | MVarTake
   | MVarRead
   | MVarPut
and formula = name * term list

type toplevel_entry = 
  | Belief of formula * formula list
  | Clause of clause

let clause_name { triggering_event } = fst triggering_event.formula
let clause_args { triggering_event } = snd triggering_event.formula
