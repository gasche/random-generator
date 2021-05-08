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

module Random
  : Sig with type 'a t = Random.State.t -> 'a
         and type 'a run = 'a
= struct
  type 'a t = Random.State.t -> 'a
  type 'a run = 'a

  let run m =
    let st = Random.State.make_self_init () in
    m st

  let map f m = fun st -> f (m st)

  let return v = fun _st -> v
  let pair m1 m2 = fun st -> (m1 st, m2 st)

  let join mm st = mm st st

  let rec fix derec_gen param =
    fun random -> derec_gen (fix derec_gen) param random

  let bool = Random.State.bool
  let int_exclusive n = fun st -> Random.State.int st n
end

module Distr
  : Sig with type 'a t = (Q.t * 'a) Seq.t
         and type 'a run = ((int*int) * 'a) list
= struct
  type 'a t = (Q.t * 'a) Seq.t
  type 'a run = ((int*int) * 'a) list

  let run m =
    m
    |> Seq.map (fun (w,v) -> (Q.view w, v))
    |> List.of_seq

  let map f m = Seq.map (fun (w, v) -> (w, f v)) m

  let return v = Seq.cons (Q.frac 1 1, v) Seq.empty
  let pair ma mb =
    ma |> Seq.flat_map @@ fun (wa, a) ->
    mb |> Seq.map @@ fun (wb, b) ->
    Q.mul wa wb, (a, b)

  let join mm =
    mm |> Seq.flat_map @@ fun (wm, m) ->
    m |> Seq.map @@ fun (w, v) ->
    Q.mul wm w, v

  let rec fix derec_gen param =
    derec_gen (fix derec_gen) param

  let bool =
    Seq.empty
    |> Seq.cons (Q.frac 1 2, true)
    |> Seq.cons (Q.frac 1 2, false)

  let int_exclusive n =
    let q = Q.frac 1 n in
    let rec loop i n =
      if i = n then Seq.empty
      else fun () ->
        Seq.Cons ((q, i), loop (i + 1) n)
    in loop 0 n
end
