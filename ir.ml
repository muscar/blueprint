open Utils
open Common
open Ast

module StringSet = Set.Make(String)

type ir =
  | IRInt of int
  | IRString of string
  | IRLoad of ir_ref_type * name
  | IRSet of ir_ref_type * name * ir
  | IRCond of ir * ir list * ir list
  | IRNew of string * ir list
  | IRCall of string * ir list
  | IRBind of name * ir * ir
  | IRReturn
and ir_ref_type = IRFieldRef | IRParamRef | IRLocalRef

type symtab = (name, ir_ref_type) Symtab.t

type ir_object = { fields: ir_field list;
				   methods: ir_method list }
and ir_field = name * ir list
and ir_method = name * ir_parameter list * ir_local list * ir list
and ir_parameter = name * index
and ir_local = name

let rec ir_of_term st = function
  | TNumber i -> IRInt i
  | TString s -> IRString s
  | TAtom a -> IRNew (a, [])
  | TVariable v ->
	(match Symtab.lookup st v with
	| Some ref_ty -> IRLoad (ref_ty, v)
	| _ -> failwith ("not bound: " ^ v))
  | TStructure (name, args) -> IRNew (name, List.map (ir_of_term st) args)

let ir_of_belief st ((name, args), metadata) =
  (name, List.map (ir_of_term st) args)

let ir_of_plan st (name, clauses) =
  let rec term_vars = function
	| TVariable v -> [v]
	| TStructure (_, args) -> args |> List.map term_vars |> List.concat
	| _ -> [] in
  let te_vars c = c |> clause_args |> List.map term_vars |> List.concat in
  let body_vars c = List.map (fun (_, (_, terms)) ->
	terms |> List.map term_vars |> List.concat) c.body |> List.concat in
  let clause_vars c = te_vars c @ body_vars c in
  let plan_vars clauses = clauses |> List.map clause_vars
                                  |> List.concat
                                  |> List.fold_left (flip StringSet.add) StringSet.empty
								  |> StringSet.elements in
  let params = List.mapi (fun idx _ ->
	("param" ^ (string_of_int idx), idx)) (clause_args (List.hd clauses)) in
  let locals = plan_vars clauses in
  let ir_of_triggering_event te =
	List.mapi (fun idx arg ->
	  IRCall ("$unify", [IRLoad (IRParamRef, fst (List.nth params idx));
						 ir_of_term st arg])) (snd te.formula) in
  let rec ir_of_context = function
	| TStructure ("$and", args) -> IRCall ("$and", List.map ir_of_context args)
	| term -> ir_of_term st term in
  let ir_of_plan_action = function
	| (Do, f) -> IRCall (fst f, List.map (ir_of_term st) (snd f))
	| (Call, f) -> IRCall (fst f, List.map (ir_of_term st) (snd f))
	| (AsyncCall, f) -> IRCall (fst f, List.map (ir_of_term st) (snd f))
	| (MVarTake, f) -> IRCall (fst f, List.map (ir_of_term st) (snd f))
	| (MVarRead, f) -> IRCall (fst f, List.map (ir_of_term st) (snd f))
	| (MVarPut, f) -> IRCall (fst f, List.map (ir_of_term st) (snd f)) in
  let ir_of_clause c =
	let te_test = ir_of_triggering_event c.triggering_event in
	let ctx_test = [ir_of_context c.context] in
	let context = (try
					 let ctx = List.assoc "context" (clause_annotations c) in
					 match ctx with
					 | [TAtom ctx_name] -> ctx_name
					 | _ -> "default"
	  with Not_found -> "default") in
	(IRCall ("$and", te_test @ ctx_test),
	 [List.fold_right (fun action ir ->
	   IRBind (context, ir_of_plan_action action, ir)) c.body IRReturn]) in
	 (* List.map ir_of_plan_action c.body) in *)
  Symtab.enter_scope st;

  List.iter (fun (param, _) -> Symtab.register st param IRParamRef) params;
  List.iter (fun param -> Symtab.register st param IRLocalRef) locals;

  let (pclauses, nclauses) = List.partition (fun clause ->
	clause.triggering_event.event_type = Add) clauses in
  let default_clause = (match nclauses with
	| [] -> (IRCall ("$error", [IRString ("plan " ^ name ^ " failed")]))
	| [c] | c::_ -> let test, body = ir_of_clause c in
					IRCond (test, body, [IRCall ("$error", [IRString ("plan " ^ name ^ " failed")])])) in
  let unbind_locals = List.map (fun local ->
	IRCall ("$unbind", [IRLoad (IRLocalRef, local)])) locals in
  let body = List.fold_left (fun alt c ->
	let test, body = ir_of_clause c in
	unbind_locals @ [IRCond (test, body, alt)]) [default_clause] pclauses in
  let meth = (name, params, locals, body) in
  Symtab.exit_scope st;
  meth

let ir_of_ast ast =
  let module StringSet = Set.Make(String) in
  let clauses = Hashtbl.create 10 in
  let rec loop beliefs names = function
	| [] -> (beliefs, (StringSet.fold (fun n c ->
	  (n, Hashtbl.find_all clauses n)::c) names []))
	| (Ast.Belief ((name, args), metadata))::ns ->
	  loop (((name, args), metadata)::beliefs) names ns
	| (Ast.Clause c)::ns ->
	  let name = Ast.clause_name c in
	  Hashtbl.add clauses name c;
	  loop beliefs (StringSet.add name names) ns in
  let beliefs, plans = loop [] StringSet.empty ast in
  let st = Symtab.empty () in
  (* XXX We should also exit this scope, but it's the agent scope so
   *     it doesn't really matter. *)
  Symtab.enter_scope st;
  { fields = List.map (ir_of_belief st) beliefs;
	methods = List.map (ir_of_plan st) plans }
