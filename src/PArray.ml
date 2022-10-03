module M = Map.Make(Int)

type 'a t = int * 'a M.t

let init len f =
  (* todo check if a balanced version could be faster *)
  let rec loop f i m =
    if i < 0 then m
    else loop f (i - 1) (M.add i (f i) m)
  in
  (len, loop f (len - 1) M.empty)

let length (len, _m) = len

let get i (len, m) =
  if i < 0 || i >= len then invalid_arg "PArray.get";
  M.find i m

let set i v (len, m) =
  if i < 0 || i >= len then invalid_arg "PArray.set";
  (len, M.add i v m)

let to_list (_len, m) =
  M.bindings m

let of_list li =
  let m = M.of_seq (List.to_seq li) in
  let rec check m i =
    if i < 0 then ()
    else if not (M.mem i m) then invalid_arg "PArray.of_seq"
    else check m (i - 1)
  in
  let len = List.length li in
  check m (len - 1);
  (len, m)
