let rec pascal n =
  match n with
  |(x, y) -> if y = 0 || x = y then 1
              else if y = 1 || x - y = 1 then x 
              else let n = (x-1, y-1) in
                    let m = (x-1, y) in
                     pascal n + pascal m