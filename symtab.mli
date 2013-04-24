type ('a, 'b) t

val empty : unit -> ('a, 'b) t

val enter_scope : ('a, 'b) t -> unit

val exit_scope : ('a, 'b) t -> unit

val register : ('a, 'b) t -> 'a -> 'b -> unit

val register_many : ('a, 'b) t -> ('a * 'b) list -> unit

val lookup : ('a, 'b) t -> 'a -> 'b option

val intern : ('a, 'b) t -> 'a -> 'b -> 'b

val in_scope : (('a, 'b) t -> 'c) -> ('a, 'b) t -> 'c
