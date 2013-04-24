open Utils

type ('a, 'b) t = (('a, 'b) Hashtbl.t) Stack.t

let empty () = Stack.create ()

let enter_scope st = Stack.push (Hashtbl.create 10) st

let exit_scope st = Stack.pop st |> ignore

let register st name data =
  Hashtbl.add (Stack.top st) name data

let register_many st pairs =
  List.iter (uncurry (register st)) pairs

let lookup st name =
  let e = ref None in
  Stack.iter (fun scope ->
	try
	  e := Some (Hashtbl.find scope name)
	with Not_found -> ()) st;
  !e

let intern st name data =
  try
	Hashtbl.find (Stack.top st) name
  with Not_found -> register st name data; data

let in_scope f = fun st ->
  enter_scope st;
  let res = f st in
  exit_scope st;
  res
