module Generator =
  Random_generator.Generator.Make(Random_generator.Prob_monad.Random)
open Generator

type term =
  | Var of var
  | Lam of var * term
  | App of term * term
and var = string

(** x, y or z *)
let random_var = Gen.(string (return 1) (char 'x' 'z' Incl))

(** a first random term generator; no well-scoping property, though *)
let random_term : term Gen.t FuelGen.t =
  let open Gen in
  let module Fuel = FuelGen in
  Fuel.fix (fun random_term () ->
    Fuel.choose [
      Fuel.nullary (let+ v = random_var in Var v);
      Fuel.unary (random_term ()) (fun tgen ->
        let+ v = random_var and+ t = tgen in Lam (v, t));
      Fuel.binary (random_term ()) (random_term ()) (fun t1 t2 ->
        let+ t1 and+ t2 in App (t1, t2));
    ]
  ) ()

(** random generator for well-scoped terms only *)
let random_scoped_term : term Gen.t FuelGen.t =
  let open Gen in
  let module Fuel = FuelGen in
  Fuel.fix (fun random_term env ->
    Fuel.choose [
      (if env = [] then Fuel.choose []
      else Fuel.nullary (let+ v = Li.select env in Var v));
      ((* in this case we want to generate a variable name
          that is then passed as paramter to the recursive generator;
          this is done by building a term of type (... Fuel.t Gen.t)
          and then using the Fuel.join_in_gen functor to go back
          to Fuel.t *)
        Fuel.join_in_gen @@
         let+ v = random_var in
         Fuel.unary (random_term (v :: env)) (fun t ->
           let+ t in Lam (v ,t)));
      Fuel.binary (random_term env) (random_term env) (fun t1 t2 ->
        let+ t1 and+ t2 in App (t1, t2));
    ]
  ) []

