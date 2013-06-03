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
	| IRLoad (ty, name) -> name
	| IRSet (ty, name, value) -> sprintf "%s = %s" name (js_of_ir value)
	| IRCond (test, cons, alt) -> sprintf "if (%s) { %s } else { %s }" (js_of_ir test) (js_of_ir_seq cons) (js_of_ir_seq alt)
	| IRNew ("true", []) -> "true"
	| IRNew ("false", []) -> "true"
	| IRNew (name, []) ->
	  let name' = "$" ^ name in
	  intern_term name' |> ignore;
	  sprintf "new $term(%s)" name'
	| IRNew (name, args) ->
	  let name' = "$" ^ name in
	  intern_term name' |> ignore;
	  sprintf "new $term(%s, %s)" name' (js_of_ir_seq args ~sep:", ")
	| IRCall ("$and", args) -> js_of_ir_seq args ~sep:" && "
	| IRCall ("$or", args) -> js_of_ir_seq args ~sep:" || "
	| IRCall (name, args) -> sprintf "%s(%s)" name (js_of_ir_seq args ~sep:", ")
  and js_of_ir_seq ?sep:(s="; ") ir_seq = String.concat s (List.map js_of_ir ir_seq)

  let js_of_ir_field (name, args) =
	sprintf "this.%s = %s" name (js_of_ir_seq ~sep:", " args)

  let js_of_ir_method (name, params, locals, body) =
	let js_params = String.concat ", " (List.map fst params) in
	let js_locals = locals |> List.map (sprintf "var %s = new $term(-1)")
	                       |> String.concat "; " in
	let js_body = js_of_ir_seq body in
	sprintf "this.%s = function (%s) { %s; %s }" name js_params js_locals js_body

  let js_of_ir_object { fields; methods } =
	Symtab.enter_scope term_ids;
	let fs = fields |> List.map js_of_ir_field |> String.concat "; " in
	let ms = methods |> List.map js_of_ir_method |> String.concat "; " in
	let term_cts = Symtab.current term_ids |> Symtab.map_scope (sprintf "var %s = %d") |> String.concat "; " in
	sprintf "%s; function agent() { %s; %s; }" term_cts fs ms

  let emit chan = output_string chan >> js_of_ir_object
end
