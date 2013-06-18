open Utils
open Common
open Ast

module StringSet = Set.Make(String)

type ir =
  | IRInt of int
  | IRString of string
  | IRIgnore
  | IRLoad of ir_ref_type * name
  | IRSet of ir_ref_type * name * ir
  | IRCond of ir * ir list * ir list
  | IRNew of string * ir list
  | IRCall of string * ir list
  | IRNativeCall of string * ir list
  | IRBind of name * ir * ir list
  | IRReturn of name
and ir_ref_type = IRFieldRef | IRParamRef | IRLocalRef

type symtab = (name, ir_ref_type) Symtab.t

type ir_object = { name: string;
		   fields: ir_field list;
		   methods: ir_method list }
and ir_field = name * ir list
and ir_method = name * ir_parameter list * ir_local list * ir list
and ir_parameter = name * index
and ir_local = name

let rec ir_of_term st = function
  | TNumber i -> IRInt i
  | TString s -> IRString s
  | TAtom a -> IRNew (a, [])
  | TVariable "_" -> IRIgnore
  | TVariable v ->
    (match Symtab.lookup st v with
      | Some ref_ty -> IRLoad (ref_ty, v)
      | _ -> failwith ("not bound: " ^ v))
  | TBinOp (op, l, r) -> IRCall (op, [ir_of_term st l; ir_of_term st r])
  | TStructure (name, args) -> IRNew (name, List.map (ir_of_term st) args)

let ir_of_belief st ((name, args), metadata) =
  (name, List.map (ir_of_term st) args)

let ir_of_plan st (name, clauses) =
  let rec term_vars = function
    | TVariable v when v <> "_" -> [v]
    | TStructure (_, args) -> args |> List.map term_vars |> List.concat
    | _ -> [] in
  let te_vars c = c |> clause_args |> List.map term_vars |> List.concat in
  let rec body_vars c = plan_stmt_vars c.body
  and plan_stmt_vars = function
    | ActionNop -> []
    | Action act -> action_vars act
    | ActionSeq (act, stmt) | ActionDo (act, stmt) ->
      action_vars act @ plan_stmt_vars stmt
  and action_vars (_, (_, terms)) =
    terms |> List.map term_vars |> List.concat in
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
	(* XXX Hardcoded `$self` *)
    | (Call, f) -> IRCall ("$self." ^ (fst f), List.map (ir_of_term st) (snd f))
    | (AsyncCall, f) -> IRCall (fst f, List.map (ir_of_term st) (snd f))
    | (ActionCall, f) -> IRNativeCall (fst f, List.map (ir_of_term st) (snd f))
    | (MVarTake, f) -> IRCall (fst f, List.map (ir_of_term st) (snd f))
    | (MVarRead, f) -> IRCall (fst f, List.map (ir_of_term st) (snd f))
    | (MVarPut, f) -> IRCall (fst f, List.map (ir_of_term st) (snd f)) in
  let rec ir_of_plan_stmt context = function
    | ActionNop -> []
    | Action act -> [ir_of_plan_action act]
    | ActionDo (act, stmt) -> [IRBind (context, ir_of_plan_action act, ir_of_plan_stmt context stmt)]
    | ActionSeq (act, stmt) -> (ir_of_plan_action act) :: ir_of_plan_stmt context stmt in
  let ir_of_clause c =
    let te_test = ir_of_triggering_event c.triggering_event in
    let ctx_test = [ir_of_context c.context] in
    let context = (try
		     let ctx = List.assoc "context" (clause_annotations c) in
		     match ctx with
		       | [TAtom ctx_name] -> ctx_name
		       | _ -> "default"
      with Not_found -> "default") in
    (IRCall ("$and", te_test @ ctx_test), ir_of_plan_stmt context c.body) in
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
    IRNativeCall (Printf.sprintf "%s.unbind" local, [IRIgnore])) locals in
  let body = List.fold_left (fun alt c ->
    let test, body = ir_of_clause c in
    unbind_locals @ [IRCond (test, body, alt)]) [default_clause] pclauses in
  let meth = (name, params, locals, body) in
  Symtab.exit_scope st;
  meth

let ir_of_ast name ast =
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
  { name = name;
    fields = List.map (ir_of_belief st) beliefs;
    methods = List.map (ir_of_plan st) plans }
