(*
 * Random_generator -- a combinator library to generate random values
 * Copyright (C) 2008-2012 Xavier Clerc
 *               2013      Gabriel Scherer
 *
 * This library evolved from experiments on the Random_generator module of Xavier Clerc's
 * Kaputt library: http://kaputt.x9c.fr/
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * - Redistributions of source code must retain the above copyright notice, this
 * list of conditions and the following disclaimer.
 * - Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the
 * distribution.
 *
 * This software is provided by the copyright holders and contributors "as is" and
 * any express or implied warranties, including, but not limited to, the implied
 * warranties of merchantability and fitness for a particular purpose are
 * disclaimed. In no event shall the copyright holder or contributors be liable
 * for any direct, indirect, incidental, special, exemplary, or consequential
 * damages (including, but not limited to, procurement of substitute goods or
 * services; loss of use, data, or profits; or business interruption) however
 * caused and on any theory of liability, whether in contract, strict liability,
 * or tort (including negligence or otherwise) arising in any way out of the use
 * of this software, even if advised of the possibility of such damage.
 *)

(** {1 Random_generator is a combinator library to generate random values. } *)

module Make (Prob : Prob_monad.Sig) : sig
(** {2 Value generators and combinators} *)

type 'a gen = 'a Prob.t
val run : 'a gen -> 'a Prob.run

type bound = Incl | Excl

val char : char -> char -> bound -> char gen
val lowercase : char gen
val uppercase : char gen
val digit : char gen

val unit : unit gen

val bool : bool gen

val prod : 'a gen -> 'b gen -> ('a * 'b) gen

val int : int -> int -> bound -> int gen

val string : int gen -> char gen -> string gen

val split_int : int -> (int * int) gen
(** [split_int n] returns two integers [(i,k)] each in the interval
    [[0;n]] such that [i + k = n] *)

(** list combinators *)

val traverse_list : 'a gen list -> 'a list gen
val shuffle_list : 'a list -> 'a list gen
(** returns a (uniform) permutation of the list *)

type 'a nonempty_list = 'a list
(** list combinators being of the form "pick an element such that",
    and random generators being total (they cannot fail), we will
    often require input lists to be non-empty. For convenience
    reasons, we won't statically enforce this, only use
    [nonempty_list] as a reminder of the implicit invariant. *)

val select : 'a nonempty_list -> 'a gen
val choose : 'a gen nonempty_list -> 'a gen

(* pure arrays *)
module PArray : sig
  type 'a t
  val init : int -> (int -> 'a) -> 'a t
  val length : 'a t -> int
  val get : int -> 'a t -> 'a
  val set : int -> 'a -> 'a t -> 'a t
  val to_list : 'a t -> (int * 'a) list
  val of_list : (int * 'a) list -> 'a t

  val traverse : 'a gen t -> 'a t gen
  val shuffle : 'a t -> 'a t gen
end

(** {2 ['a gen] is a functor} *)

val map : ('a -> 'b) -> 'a gen -> 'b gen
val (let+) : 'a gen -> ('a -> 'b) -> 'b gen

(** The functor's [map] is very useful to post-process the result of
    a random generator.
{[
    let hexa_digit =
       let+ i = make_int 0 16 in
       "0123456789ABCDEF".[i]
]}
*)

(** {2 ['a gen] is applicative} *)

val return : 'a -> 'a gen

val pair : 'a gen -> 'b gen -> ('a * 'b) gen
val ( and+ ) : 'a gen -> 'b gen -> ('a * 'b) gen

(** Applicative combinators are useful to lift a function applied to
    pure arguments into a function applied to generators. You would
    naturally write [Array.make n v] to initialize an array. If the
    length and the initial values are instead randomly generated
    ([foo : int gen] and [bar : 'a gen]), then you can mechanically write
    [let+ n = foo and+ v = bar in Array.make f n v].
*)

(** {2 ['a gen] is a monad} *)

(** synonym of [pure] *)
val join : 'a gen gen -> 'a gen
val bind : ('a -> 'b gen) -> 'a gen -> 'b gen
val ( let* ) : 'a gen -> ('a -> 'b gen) -> 'b gen
val ( and* ) : 'a gen -> 'b gen -> ('a * 'b) gen

(** Monad combinators are useful when you need an intermediate random
    result to return not a random value ([map] is enough for this) but
    a random {i generator} -- the generator is itself picked
    randomly. Applicative is often enough in practice, as you can
    usually statically decide the structure of your random generator,
    and build it by applying pure functions to random inputs.

    For an example of [bind], consider you want to generate any
    positive integer, but you prefer to generate small numbers more
    often (they're easier to work with). One idea is to first draw
    a boolean at random, then decide what kind of number to generate
    based on it.
{[
    let my_gen =
      let* small = bool in
      if small then int 0 10
      else int 0 10_000
]}

    Remark: the kind of logic used in this example
    (picking a generator among a finite set) is encapsulated in the
    {!choose} function.
{[
    let my_gen = choose [int 0 10; int 0 10_000]
]}
*)


(** {2 parametrized fixpoint} *)

val fix : (('a -> 'b gen) -> ('a -> 'b gen)) -> 'a -> 'b gen

(** Parametrized fixpoint is more expressive than non-parametrized
    fixpoint [('b gen -> 'b gen) -> 'b gen], as it encodes recursive
    calls with varying arguments. Consider a variant of the factorial
    function defined by the following pseudo-code:
{[
    let rec fact = function
    | 0 -> 1 or -1 (*at random*)
    | n -> n * fact (n - 1)
]}
    Encoding it as a fixpoint is direct, the trick is to name the
    weird function argument 'fact' as well, and use it in place of
    recursive calls:
{[
    let fact = fix (fun fact -> function
      | 0 -> select [1; -1]
      | n -> let+ v = fact (n - 1) in n * v
]}
*)

(** {2 backtracking generator} *)

type 'a backtrack_gen = 'a option gen
(** Represents generator that may fail. *)

(** It is quite common when doing random testing to ask for an element
    randomly generated by [gen], but only one that satisfies some
    predicate [p]. This can be expressed simply as [succeed gen |>
    guard p |> backtrack].
*)

val succeed : 'a gen -> 'a backtrack_gen
val guard : ('a -> bool) -> 'a backtrack_gen -> 'a backtrack_gen
val cond : bool -> 'a backtrack_gen -> 'a backtrack_gen
(** [cond b gen] should be preferred to [guard (fun _ -> b) gen] for
    performance reasons, as the generator is not requested any output
    in the [false] case. *)

val backtrack : 'a backtrack_gen -> 'a gen
(** [backtrack gen] will loop until the generator returns a successful
    value [Some v]. This may not terminate, and more generally can be
    a performance concern. You should test validity as locally as
    possible. *)


(** {2 fueled generators} *)

(** Fueled generators are domain-specific generators aimed at
    producing "good-looking" values that have a size and are built
    from subcomponents -- in particular values of algebraic types.

    Consider a type of binary trees:
{[
    type 'a tree = Leaf of 'a | Node of 'a tree * 'a tree
    let _Leaf v = Leaf v
    let _Node t1 t2 = Node (t1, t2)
]}

    You can generate trees by using [fix], but how would you control
    the size of the produced tree? You could flip a coin each time to
    decide whether to produce a [Leaf] or a [Node], and then hope that
    as probability to get to depth N decreases exponentially, your
    trees will not be too large in practice.

    The problem with most of those obvious methods is that you will
    get "bad-looking" terms, with highly skewed branches, either much
    smaller or much larger than you expected, etc. This is probably
    fine to exercize your code, but becomes problematic when you find
    a counter-example of the property you're testing, and cannot
    display and look at it without getting a headache.

    Fuel is a generic technique to generate "good-looking" terms. The
    idea is that instead of trying to limit the height of tree
    branches, you randomly pick in advance the size (number of nodes)
    of the tree you will generate (with the distribution of
    your choice), then thread an "amount of fuel left" to each
    recursive subcall representing their part of the tree should have;
    when generating several branches, you want the fuel to be split
    randomly between each.

    If you want to generate two branches of the tree, you may think of
    first passing the whole fuel you have to the generator of the left
    branch, consider it will not consume everything, and pass the rest
    to the right branch. This is not what this library does: we decide
    in advance which amount of fuel each branch should consume
    (such that the sum is the total amount available, usually minus
    one to account for the node's cost itself), and then force each
    branch to build a term {i exactly of this size}.

    A {i fueled generator} ['a fueled] is a function that, given
    some amount of fuel, will randomly generate an ['a option]
    by consuming exactly this amount of fuel -- if there is no
    term of the requested size, it returns [None].
*)
module Fueled : sig
  type 'a t = int -> 'a backtrack_gen

  val map : ('a -> 'b) -> 'a t -> 'b t
  val (let+) : 'a t -> ('a -> 'b) -> 'b t
  val (and+) : 'a t -> 'b t -> ('a * 'b) t

  val zero : 'a -> 'a t
  val tick : 'a t -> 'a t
  val prod : (int -> (int * int) gen) ->
    'a t -> 'b t -> ('a * 'b) t

  val choose : 'a t list -> 'a t
  (** For a given amount of fuel, we will only choose among the
      generators that can produce a term. *)

  val fix : (('a -> 'b t) -> ('a -> 'b t)) -> ('a -> 'b t)
end
(**
    The [Fueled] submodule provides the basic operations on fueled
    generators.
*)

(**
    The library provides you with the basic constructions to build
    fueled generators; in particular, you have to apply the [tick]
    function yourself at places that you think should consume one unit
    of fuel, and can provide your own splitting function to say how to
    divide fuel between subbranches (if you have a simple situation
    with only two branches, you can use {!split_int}).

    We also provide some convenience functions outside
    this module, {!nullary}, {!unary} and {!binary}, that have
    a pre-built tick+split logic that will suit most practical use
    cases for generating inductive types with constructors having
    zero, one or two recursive occurences.

    Remark: I have no idea what the distribution of terms generated
    used this technique is, and whether it is "uniform" -- probably
    not. I only found it very useful in practice.

    Here is a fueled generator for [unit tree]:
{[
    let tree : unit tree Fueled.t =
      Fueled.fix (fun tree () ->
        Fueled.choose [
          nullary (Leaf ());
          binary (tree ()) (tree ()) _Node;
        ]) ()
]}

    Now consider producing a [bool tree], with each leaf boolean
    picked at random. An important thing to understand is that Fueled's
    duty and scope is to pick a random {i shape} for your data
    structure, not help you select the values of the non-recursive
    part of the program (randomly or not). The right way to do that is
    to actually build a [bool tree gen fueled], that is a fueled
    generator of {i random boolean trees} of a given shape (decided by
    the fuel engine); so we have one level of randomness for the
    shape, one for the leaf values.
{[
    let random_tree : bool tree gen Fueled.t =
      Fueled.fix (fun random_tree () ->
        Fueled.choose [
          nullary (let+ b = bool in Leaf b);
          binary (random_tree ()) (random_tree ())
            (fun t1 t2 -> let+ t1 = t1 and+ t2 = t2 in Node(t1, t2));
        ]) ()

    let tree (size : int gen) : bool tree gen =
      let* n = size in
      random_tree n |> backtrack |> join
]}
*)

(** {3 convenience functions for fueled generators} *)

val nullary : 'a -> 'a Fueled.t
(** zero *)

val unary : 'a Fueled.t -> ('a -> 'b) -> 'b Fueled.t
(** will do a tick *)

val binary : 'a Fueled.t -> 'b Fueled.t -> ('a -> 'b -> 'c) -> 'c Fueled.t
(** will do a tick and use split_int *)
end
