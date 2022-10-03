(** Persistent arrays *)

type 'a t
val init : int -> (int -> 'a) -> 'a t
val length : 'a t -> int
val get : int -> 'a t -> 'a
val set : int -> 'a -> 'a t -> 'a t
val to_list : 'a t -> (int * 'a) list
val of_list : (int * 'a) list -> 'a t
