let rec closest s x =
  let rec min (d, x) =
    if fst d <= fst x then snd d
    else snd x
  in let rec dis(a, b) = 
       let len_a = String.length a in
       let len_b = String.length b in
       if b = "" then len_a
       else if a = "" then len_b
       else if String.sub a 0 1 = String.sub b 0 1 then dis(String.sub a 1 (len_a-1), String.sub b 1 (len_b-1))
       else 
         let x = dis(String.sub a 1 (len_a-1), String.sub b 0 len_b) in
         let y = dis(String.sub a 0 len_a, String.sub b 1 (len_b-1)) in
         let z = dis(String.sub a 1 (len_a-1), String.sub b 1 (len_b-1)) in
         if x > y || x > z then
           if y > z then z+1
           else y+1
         else x+1
  in match x with
  |[] -> snd (0, s)
  |hd :: [] -> snd (dis(s, hd), hd)
  |hd :: tl -> min ((dis(s, hd), hd), (dis(s, closest s tl), closest s tl))
