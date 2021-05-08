type t

val frac : int -> int -> t
val view : t -> int * int

val equal : t -> t -> bool
val compare : t -> t -> int

val add : t -> t -> t
val sub : t -> t -> t
val mul : t -> t -> t
val div : t -> t -> t

module Ops : sig
  val ( =! ) : t -> t -> bool
  val ( +! ) : t -> t -> t
  val ( -! ) : t -> t -> t
  val ( *! ) : t -> t -> t
  val ( /! ) : t -> t -> t
end
