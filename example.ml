type term =
  | Var of var
  | Lam of var * term
  | App of term * term
and var = string

(** x, y or z *)
let random_var = Generator.(string (return 1) (make_char 'x' 3))

(** a first random term generator; no well-scoping property, though *)
let random_term =
  let open Generator in
  let ($$) = app in
  Fuel.fix (fun random_term () ->
    Fuel.choose [
      nullary (map (fun v -> Var v) random_var);
      unary (random_term ()) (fun tgen ->
        pure (fun v t -> Lam (v, t)) $$ random_var $$ tgen);
      binary (random_term ()) (random_term ()) (fun tgen1 tgen2 ->
        pure (fun t1 t2 -> App (t1, t2)) $$ tgen1 $$ tgen2);
    ]
  )

(** random generator for well-scoped terms only *)
let random_scoped_term =
  let open Generator in
  let ($$) = app in
  Fuel.fix (fun random_term env ->
    Fuel.choose [
      if env = [] then return (fun _ -> None)
      else nullary (pure (fun v -> Var v) $$ select env);
      bind' random_var (fun v ->
        unary (random_term (v :: env))
          (fun tgen -> pure (fun t -> Lam (v, t)) $$ tgen));
      binary (random_term env) (random_term env) (fun tgen1 tgen2 ->
        pure (fun t1 t2 -> App (t1, t2)) $$ tgen1 $$ tgen2);
    ]
  )

