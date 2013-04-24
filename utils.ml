let ( >> ) f g h = f (g h)

let ( |> ) x f = f x

let ( **> ) f x = f x

let curry f = fun x y -> f (x, y)

let uncurry f = fun (x, y) -> f x y

let const f x = fun _ -> f x

let flip f x y = f y x

let pair x y = (x, y)

module List =
struct
  include List

  let init n f =
	let rec loop acc = function
	  | 0 -> acc
	  | n -> loop ((f n)::acc) (n - 1) in
	loop [] n

  let rec filter_map f = function
	| [] -> []
	| x::xs ->
	  match f x with
	  | Some y -> y::filter_map f xs
	  | _ -> filter_map f xs
end
