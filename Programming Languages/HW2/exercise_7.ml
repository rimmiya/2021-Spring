type relationships = (string * string) list;;
type person = string;;

let rec count l =
  match l with 
    [] -> 0
  |hd :: [] -> if List.exists (fun x -> x = List.hd l) (List.tl l) then 0
      else 1
  |hd :: tl ->
      if List.exists (fun x -> x = hd) (tl) then count tl
      else 1 + count tl
    
         
let rec likes : relationships -> person -> int = fun r p ->
  let rec f r p li rr=
    match r with
    |[] -> li 
    |hd :: tl -> 
        if p = fst hd then
          if li = [] then let li = [snd (List.hd r)] in li @ (f tl p li rr) @ (f rr (snd (List.hd r)) li rr)
          else 
          if List.exists (fun x -> x = snd (List.hd r)) li then f tl p li rr
          else let lis = li @ [snd (List.hd r)] in [snd (List.hd r)] @ (f tl p lis rr) @ (f rr (snd (List.hd r)) lis rr)
        else f tl p li rr
  in count (f r p [] r);;