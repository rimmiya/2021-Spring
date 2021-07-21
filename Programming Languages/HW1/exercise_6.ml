type btree = Empty | Node of int * btree * btree

let rec balanced n =
  let rec depth d =
    match d with
    Empty -> 0
    |Node (_, l, r) -> if (depth l) > (depth r) then (depth l) + 1 else (depth r) + 1
  in match n with
  Empty -> true
  |Node (_, l, r) -> if (((depth l) - (depth r) <= 1) && ((depth l) - (depth r) >= -1)) && (balanced l) && (balanced r) then true else false;;