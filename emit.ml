open Utils
open Printf
open Ir

let rec js_of_ir = function
  | IRInt i -> sprintf "%d" i
  | IRString s -> sprintf "\"%s\"" s
  | IRLoad (ty, name) -> name
  | IRSet (ty, name, value) -> sprintf "%s = %s" name (js_of_ir value)
  | IRCond (test, cons, alt) -> sprintf "if (%s) { %s } else { %s }" (js_of_ir test) (js_of_ir_seq cons) (js_of_ir_seq alt)
  | IRNew (name, args) -> sprintf "new %s(%s)" name (js_of_ir_seq args ~sep:", ")
  | IRCall (name, args) -> sprintf "%s(%s)" name (js_of_ir_seq args ~sep:", ")
and js_of_ir_seq ?sep:(s="; ") ir_seq = String.concat s (List.map js_of_ir ir_seq)

let js_of_ir_field (name, args) = sprintf "this.%s = new $tuple(%s)" name (js_of_ir_seq ~sep:", " args)

let js_of_ir_method (name, params, body) =
  sprintf "this.%s = function (%s) { %s }" name (String.concat ", " (List.map fst params)) (js_of_ir_seq body)

let js_of_ir_object { fields; methods } =
  let fs = List.map js_of_ir_field fields in
  let ms = List.map js_of_ir_method methods in
  sprintf "function agent() { %s }" ((String.concat "; " fs) ^ "; " ^ (String.concat "; " ms))
