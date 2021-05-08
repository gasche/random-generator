(* A naive implementation of rational numbers. This is just for prototyping purposes. *)

type t = int * int

let norm ((a, b) as q) =
  let rec gcd a b =
    if b = 0 then a
    else gcd b (a mod b)
  in
  let d = gcd (abs a) (abs b) in
  if d = 1 && (a >= 0 || b > 0) then q
  else
    let a = a / d and b = b / d in
    if a < 0 && b < 0
    then (-a, -b)
    else (a, b)

let frac a b =
  if b = 0 then invalid_arg "Q.frac";
  norm (a, b)

let view (a, b) = (a, b)

let equal q1 q2 = (norm q1 = norm q2)

let compare (a, b) (c, d) = Int.compare (a*d) (b*c)

let add (a, b) (c, d) = norm (a*d + b*c, b*d)
let sub (a, b) (c, d) = norm (a*d - b*c, b*d)
let mul (a, b) (c, d) = norm (a*c, b*d)
let div (a, b) (c, d) =
  if c = 0 then invalid_arg "Q.div";
  norm (a*d, b*c)

module Ops = struct
  let ( =! ) = equal
  let ( +! ) = add
  let ( -! ) = sub
  let ( *! ) = mul
  let ( /! ) = div
end
