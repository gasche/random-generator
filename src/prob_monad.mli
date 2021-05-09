module type Sig = sig
  type 'a t
  type 'a run

  val run : 'a t -> 'a run

  (** Functor *)
  val map : ('a -> 'b) -> 'a t -> 'b t

  (** Applicative *)
  val return : 'a -> 'a t
  val pair : 'a t -> 'b t -> ('a * 'b) t

  (** Monad *)
  val join : 'a t t -> 'a t

  (** Fix *)
  val fix : (('a -> 'b t) -> ('a -> 'b t)) -> 'a -> 'b t

  (** Random operations *)
  val bool : bool t
  val int_exclusive : int -> int t
end

module Random : Sig with type 'a t = Random.State.t -> 'a
                     and type 'a run = 'a

module Distr : Sig with type 'a t = (Q.t * 'a) Seq.t
                    and type 'a run = cmp:('a -> 'a -> int) ->((int*int) * 'a) list
