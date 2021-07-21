let rec contains_all l1 l2 =
  match l1 with
  | [] -> true
  | hd::tl -> if List.mem hd l2 then contains_all tl l2 else false

let equivalence a b = (contains_all a b) && (contains_all b a)

let rec cartesian l1 l2 =
  let rec product a b =
    match b with
    [] -> []
    |hb :: tb -> (a, hb) :: product a tb
  in match l1, l2 with
  [], _| _, [] -> []
  |h1 :: t1, h2 :: t2 -> product h1 l2 @ cartesian t1 l2

let test t1 t2 answer =
  let v = cartesian t1 t2 in
  (equivalence v answer);;
