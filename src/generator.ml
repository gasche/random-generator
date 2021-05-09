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

module Make (Prob : Prob_monad.Sig) = struct

  module Gen = struct
    type 'a t = 'a Prob.t
    let run = Prob.run

    let return = Prob.return

    let map = Prob.map
    let ( let+ ) m f = map f m

    let pair = Prob.pair
    let ( and+ ) = pair

    let ( let* ) m f = Prob.join (Prob.map f m)
    let ( and* ) = Prob.pair

    let join = Prob.join
    let bind f m = ( let* ) m f

    let fix = Prob.fix

    type bound = Incl | Excl

    let char start limit bound =
      let size =
        match bound with
        | Incl -> int_of_char limit - int_of_char start + 1
        | Excl -> int_of_char limit - int_of_char start
      in
      let+ i = Prob.int_exclusive size in
      char_of_int (int_of_char start + i)

    let lowercase = char 'a' 'z' Incl
    let uppercase = char 'A' 'Z' Incl
    let digit = char '0' '9' Incl

    let unit = Prob.return ()
    let bool = Prob.bool

    let prod = Prob.pair

    let int start limit bound =
      let size = match bound with
        | Incl -> limit - start + 1
        | Excl -> limit - start
      in
      let+ i = Prob.int_exclusive size in
      start + i

    let split_int n =
      let+ k = Prob.int_exclusive (n + 1) in
      (k, n - k)

    module Ar = struct
      let traverse arr =
        let rec loop = function
          | [] -> return []
          | (i, gen) :: gens ->
            let+ v = gen
            and+ vs = loop gens
            in (i, v) :: vs
        in
        let+ li = loop (PArray.to_list arr) in
        PArray.of_list li

      let select arr =
        let+ i = Prob.int_exclusive (PArray.length arr) in
        PArray.get i arr

      let choose arr =
        Prob.join (select arr)

      let shuffle arr =
        let swap i j m =
          if i = j then m
          else
            arr
            |> PArray.set i (PArray.get j arr)
            |> PArray.set j (PArray.get j arr)
        in
        let rec loop i m =
          if i <= 1 then Prob.return m
          else
            let* k = Prob.int_exclusive i in
            loop (i - 1) (swap (i - 1) k m)
        in
        loop (PArray.length arr) arr
    end

    module Li = struct
      let rec traverse = function
        | [] -> return []
        | m::ms ->
          let+ v = m
          and+ vs = traverse ms
          in v::vs

      type 'a nonempty_list = 'a list

      let select li =
        let+ i = Prob.int_exclusive (List.length li) in
        List.nth li i

      let choose li =
        Prob.join (select li)

      let shuffle li =
        let+ arr =
          li
          |> List.mapi (fun i v -> (i, v))
          |> PArray.of_list
          |> Ar.shuffle
        in
        PArray.to_list arr
        |> List.map snd
    end

    let string size char =
      let* n = size in
      let+ chars = Li.traverse @@ List.init n (fun _ -> char) in
      String.concat "" (List.map (String.make 1) chars)
  end

  (** backtracking operator *)

    module BacktrackGen = struct
      type 'a t = 'a option Gen.t

      open Gen

      let succeed gen =
        let+ x = gen in Some x

      let guard p gen =
        let+ o = gen in
        match o with
        | None -> None
        | Some x as res->
          if p x then res else None

      let cond p gen =
        (* it is important not to call (gen r) if 'p' is false, as this
         function may be used to guard cases where the random generator
         would fail on its input (e.g. a negative number passed to
         Random.State.int) *)
      if p then gen else Prob.return None

      let rec backtrack gen =
        let* o = gen in
        match o with
        | None -> backtrack gen
        | Some v -> return v
    end

  (** fueled generators *)
  module FuelGen = struct
    type 'a t = int -> 'a BacktrackGen.t

    open Gen

    let map f gen =
      fun fuel ->
        Prob.map (Option.map f) (gen fuel)

    let zero v = function
      | 0 -> Gen.return (Some v)
      | _ -> Gen.return None

    let tick gen =
      fun fuel ->
        let fuel = fuel - 1 in
        if fuel < 0 then return None
        else gen fuel

    let prod split gen1 gen2 =
      fun fuel ->
        let* (fuel1, fuel2) = split fuel in
        let+ o1 = gen1 fuel1
        and+ o2 = gen2 fuel2 in
        match o1, o2 with
        | None, _ | _, None -> None
        | Some v1, Some v2 -> Some (v1, v2)

    let choose li =
      fun fuel ->
        let* choices = Li.traverse (List.map (fun gen -> gen fuel) li) in
        match List.filter_map Fun.id choices with
        | [] -> return None
        | _::_ as choices -> let+ v = Li.select choices in Some v

    let rec fix derec_gen param =
      fun fuel -> derec_gen (fix derec_gen) param fuel

    let (let+) gen f = map f gen

    let nullary v = zero v
    let unary gen f = map f (tick gen)
    let binary gen1 gen2 merge =
      tick @@
      let+ (v1, v2) = prod Gen.split_int gen1 gen2 in
      merge v1 v2
  end

end
