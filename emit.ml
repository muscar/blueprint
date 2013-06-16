open Utils
open Printf
open Ir

module type CodeGenerator =
sig
  type t

  val emit : out_channel -> Ir.ir_object -> unit
end

module JSCodeGenerator : CodeGenerator =
struct
  type t = string

  let term_ids = Symtab.empty ()

  let next_term_id =
	let cnt = ref 0 in
	fun () ->
	  let id = !cnt in
	  incr cnt;
	  id

  let intern_term name = Symtab.intern_init term_ids name next_term_id

  let rec js_of_ir = function
	| IRInt i -> sprintf "%d" i
	| IRString s -> sprintf "\"%s\"" s
	| IRIgnore -> "_"
	| IRLoad (_, name) -> name
	| IRSet (ty, name, value) -> sprintf "%s = %s" name (js_of_ir value)
	| IRCond (test, cons, alt) -> sprintf "if (%s) { %s } else { %s }" (js_of_ir test) (js_of_ir_seq cons) (js_of_ir_seq alt)
	| IRNew ("true", []) -> "true"
	| IRNew ("false", []) -> "false"
	| IRNew ("nil", []) -> "$nil"
	| IRNew ("$cons", [head; tail]) -> sprintf "new $cons(%s, %s)" (js_of_ir head) (js_of_ir tail)
	| IRNew (name, []) ->
	  let name' = "$" ^ name in
	  intern_term name' |> ignore;
	  sprintf "new $term(%s, '%s')" name' name
	| IRNew (name, args) ->
	  let name' = "$" ^ name in
	  intern_term name' |> ignore;
	  sprintf "new $term(%s, '%s', %s)" name' name (js_of_ir_seq args ~sep:", ")
	| IRCall (op, [l; r]) when bin_op op -> sprintf "%s %s %s" (emit_load_resolve l) op (emit_load_resolve r)
	| IRCall ("$and", args) -> js_of_ir_seq args ~sep:" && "
	| IRCall ("$or", args) -> js_of_ir_seq args ~sep:" || "
	| IRCall (name, args) -> sprintf "%s(%s)" name (js_of_ir_seq args ~sep:", ")
	| IRNativeCall (name, args) ->
	  (match List.butlast args, List.last args with
	  | actualArgs, IRIgnore -> sprintf "%s(%s)" name (js_of_ir_seq actualArgs ~sep:", " ~op:emit_load_resolve)
	  | [], IRLoad (IRLocalRef, resultName) -> sprintf "$unify(%s(), %s)" name resultName
	  | actualArgs, IRLoad (IRLocalRef, resultName) -> sprintf "$unify(%s(%s), %s)" name (js_of_ir_seq actualArgs ~sep:", " ~op:emit_load_resolve) resultName
	  | _, _ -> failwith "Last argument to native call must be a variable")
	| IRReturn ctx -> sprintf "%s.unit();" ctx
	| IRBind (ctx, (IRNativeCall (name, args)), irs) ->
	  sprintf "%s.bind(%s(%s), function () { %s; })" ctx name (js_of_ir_seq args ~sep:", ") (js_of_ir_seq irs ~sep:"; ")
	  (* (match List.butlast args, List.last args with *)
	  (* | actualArgs, IRIgnore -> sprintf "%s.bind(%s, function () { %s; })" ctx (js_of_ir ir) (js_of_ir_seq irs ~sep:"; ") *)
	  (* | [], IRLoad (IRLocalRef, resultName) -> *)
	  (*   sprintf "%s.bind(%s(), function (%s) { %s; })" ctx name resultName (js_of_ir_seq irs ~sep:"; ") *)
	  (* | actualArgs, IRLoad (IRLocalRef, resultName) -> *)
	  (*   sprintf "%s.bind(%s, function (%s) { %s; })" ctx (js_of_ir (IRNativeCall (name, actualArgs))) resultName (js_of_ir_seq irs ~sep:"; ") *)
	  (* | _, _ -> failwith "Last argument to native call must be a variable") *)
	| IRBind (ctx, ir1, irs) ->
	  sprintf "%s.bind(%s, function () { %s; })" ctx (js_of_ir ir1) (js_of_ir_seq irs ~sep:"; ")
  and js_of_ir_seq ?sep:(s="; ") ?op:(op=js_of_ir) ir_seq = String.concat s (List.map op ir_seq)
  and emit_load_resolve = function
	| IRLoad (IRLocalRef, name) -> sprintf "%s.getValue()" name
	| IRLoad (_, name) -> name
	| ir -> js_of_ir ir
  and bin_op = function
	| "+" | "-" | "*" | "/" -> true
	| _ -> false

  let js_of_ir_field (name, args) =
	sprintf "this.%s = %s" name (js_of_ir_seq ~sep:", " args)

  let js_of_ir_method (name, params, locals, body) =
	let js_params = String.concat ", " (List.map fst params) in
	let js_locals = List.map (fun l -> sprintf "var %s = new $variable('%s')" l l) locals |> String.concat "; " in
	let js_body = js_of_ir_seq body in
	sprintf "this.%s = function (%s) { %s; %s }" name js_params js_locals js_body

  let js_of_ir_object { fields; methods } =
	Symtab.enter_scope term_ids;
	let fs = fields |> List.map js_of_ir_field |> String.concat "; " in
	let ms = methods |> List.map js_of_ir_method |> String.concat "; " in
	let term_cts = Symtab.current term_ids |> Symtab.map_scope (sprintf "var %s = %d") |> String.concat "; " in
	sprintf "%s; function agent() { var $self = this; %s; %s; }" term_cts fs ms

  let emit chan = output_string chan >> js_of_ir_object
end
