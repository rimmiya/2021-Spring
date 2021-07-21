let count_string s x =
  let rec cs (s, x, i) =
    if String.length s < String.length x + i then 0
    else if (String.sub s i (String.length x)) = x then 1 + (cs (s, x, i+1))
    else cs (s, x, i+1)
  in if x = "" then 0 else cs (s, x, 0)