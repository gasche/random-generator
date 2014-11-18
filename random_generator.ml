(*
 * Generator -- a combinator library to generate random values
 * Copyright (C) 2008-2012 Xavier Clerc
 *               2013      Gabriel Scherer
 *
 * This library evolved from experiments on the Generator module of Xavier Clerc's
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

type random_state = Random.State.t
type 'a gen = random_state -> 'a
let run r = r

let lift f x = fun r -> f r x

(** 'a gen is a monad *)
let map f gen = fun rand -> f (gen rand)
let map' gen f = map f gen

let app f gen = fun rand -> f rand (gen rand)
let app' gen f = app f gen

let pure f = fun _ -> f

let return x = fun _ -> x
let bind f gen = fun rand -> f (gen rand) rand
let bind' gen f = bind f gen
let join gen = fun rand -> gen rand rand

let rec fix derec_gen param =
  fun rand -> derec_gen (fix derec_gen) param rand

let prod g1 g2 =
  fun rand ->
    let v1 = g1 rand in
    let v2 = g2 rand in
    (v1, v2)

(** Value generators *)
let unit r = ()
let bool r = Random.State.bool r
let bits r = Random.State.bits r
let make_int a b r = a + Random.State.int r (b - a)
let make_float a b r = a +. Random.State.float r (b -. a)
let make_int32 a b r = Int32.(add a @@ Random.State.int32 r @@ sub b a)
let make_int64 a b r = Int64.(add a @@ Random.State.int64 r @@ sub b a)
let make_nativeint a b r = Nativeint.(add a @@ Random.State.nativeint r @@ sub b a)

let split_int n r =
  let k = Random.State.int r (n + 1) in
  (k, n - k)

let make_char start len r =
  let n = Random.State.int r len in
  char_of_int (n + int_of_char start)
let lowercase = make_char 'a' 26
let uppercase = make_char 'A' 26
let digit = make_char '0' 10

let string int char r =
  let len = int r in
  let res = String.init len (fun _ -> char r) in
  res

let shuffle li r =
  let array = Array.of_list li in
  for i = Array.length array-1 downto 1 do
    let j = Random.State.int r (i+1) in
    let tmp = array.(i) in
    array.(i) <- array.(j);
    array.(j) <- tmp;
  done;
  Array.to_list array

type 'a nonempty_list = 'a list

let select li r =
  let len = List.length li in
  List.nth li (Random.State.int r len)

let choose li = join (select li)


(** backtracking operator *)
type 'a backtrack_gen = 'a option gen

let succeed gen = map (fun x -> Some x) gen

let guard p gen r =
  match gen r with
    | None -> None
    | Some x ->
      if p x then Some x else None

let cond p gen r =
  (* it is important not to call (gen r) if 'p' is false, as this
     function may be used to guard cases where the random generator
     would fail on its input (e.g. a negative number passed to
     Random.State.int) *)
  if p then gen r else None

let rec backtrack gen r = match gen r with
  | None -> backtrack gen r
  | Some result -> result


(** fueled generators *)
type 'a fueled = (int -> 'a option) gen

module Fuel = struct
  let map f gen random fuel =
    match gen random fuel with
      | None -> None
      | Some x -> Some (f x)
  let map' f gen = map gen f

  let zero v _random = function
    | 0 -> Some v
    | _ -> None

  let tick gen random fuel =
    let fuel = fuel - 1 in
    if fuel < 0 then None
    else gen random fuel

  let prod split gen1 gen2 = fun random fuel ->
    let fuel1, fuel2 = split fuel random in
    match gen1 random fuel1, gen2 random fuel2 with
      | None, _ | _, None -> None
      | Some v1, Some v2 -> Some (v1, v2)

  let choose li random fuel =
    let li = shuffle li random in
    let rec first = function
      | [] -> None
      | x::xs ->
         begin match x random fuel with
                 | None -> first xs
                 | Some _ as result -> result
         end
    in first li

  let rec fix derec_gen param =
    fun random fuel -> derec_gen (fix derec_gen) param random fuel
end

let nullary v = Fuel.zero v
let unary gen f = Fuel.(map f (tick gen))
let binary gen1 gen2 merge =
  let open Fuel in
  map'
    (tick (prod split_int gen1 gen2))
    (fun (v1, v2) -> merge v1 v2)

