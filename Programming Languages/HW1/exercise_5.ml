type btree = Empty | Node of int * btree * btree

let rec height n =
  match n with
  Empty -> 0
  |Node (_, l, r) -> if (height l) > (height r) then (height l) + 1 else (height r) + 1;;