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

    type bound = Excl | Incl
    let of_bound = function
      | Excl -> 0
      | Incl -> 1

    let range_size start limit bound =
      limit - start + of_bound bound

    let char start limit bound =
      let size = range_size (int_of_char start) (int_of_char limit) bound in
      let+ i = Prob.int_exclusive size in
      char_of_int (int_of_char start + i)

    let lowercase = char 'a' 'z' Incl
    let uppercase = char 'A' 'Z' Incl
    let digit = char '0' '9' Incl

    let unit = Prob.return ()
    let bool = Prob.bool

    let prod = Prob.pair

    let int start limit bound =
      let size = range_size start limit bound in
      let+ i = Prob.int_exclusive size in
      start + i

    module Nat = struct
      let split2 n =
        let+ k = Prob.int_exclusive (n + 1) in
        (k, n - k)

      let subset ~size start limit bound =
        let range = range_size start limit bound in
        if not (0 <= size && size <= range) then invalid_arg "Gen.Nat.subset";
        (* The algorithm below is attributed to Floyd, see for example
           https://eyalsch.wordpress.com/2010/04/01/random-sample/
           https://math.stackexchange.com/questions/178690
        *)
        let module ISet = Set.Make(Int) in
        let stop = limit + of_bound bound in
        let rec fill set i =
          if i = stop then return set
          else
            let* pos = int start i Incl in
            let choice = if ISet.mem pos set then i else pos in
            fill (ISet.add choice set) (i + 1)
        in
        let+ set = fill ISet.empty (stop - size) in
        ISet.elements set

      let pos_split ~size:k n =
        (* To split n into n{0}+n{1}+..+n{k-1}, we draw distinct "boundaries"
           b{-1}..b{k-1}, with b{-1}=0 and b{k-1} = n
           and the k-1 intermediate boundaries b{0}..b{k-2}
           chosen randomly distinct in [1;n-1].

           Then each n{i} is defined as b{i}-b{i-1}. *)
        let+ b = subset ~size:(k-1) 1 (n - 1) Incl in
        let b = Array.of_list b in
        List.init k (fun i ->
          if i = 0 then b.(0)
          else if i = k-1 then n - b.(i-1)
          else b.(i) - b.(i-1)
        )

      let split ~size:k n =
        let+ ns = pos_split ~size:k (n+k) in
        List.map (fun v -> v - 1) ns
    end

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

      let subset size arr =
        let+ indices = Nat.subset ~size 0 (PArray.length arr) Excl in
        List.map (fun i -> (i, PArray.get i arr)) indices
        |> PArray.of_list
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

      let subset size li =
        let+ indices = Nat.subset ~size 0 (List.length li) Excl in
        let arr = Array.of_list li in
        List.map (fun i -> arr.(i)) indices
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

    let join_in_gen (gen : 'a t Gen.t) : 'a t =
      fun fuel -> Gen.(let* gen in gen fuel)

    let join_in_backtrack_gen (gen : 'a t BacktrackGen.t) : 'a t =
      fun fuel ->
        Gen.(let* gen in
             match gen with
             | None -> return None
             | Some gen -> gen fuel)

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

    let prod split2 gen1 gen2 =
      fun fuel ->
        let* (fuel1, fuel2) = split2 fuel in
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
      let+ (v1, v2) = prod Gen.Nat.split2 gen1 gen2 in
      merge v1 v2
  end

end
