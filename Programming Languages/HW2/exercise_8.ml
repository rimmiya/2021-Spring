type relationships = (string * string) list

let rec selflove r =  
  let rec ispossible r li =
    match r with
    [] -> li
    |hd :: [] -> li@[snd hd]
    |hd :: tl -> ispossible tl li@[snd hd]
  in let rec f rr r p li =
       match r with
       |[] -> li 
       |hd :: tl -> 
           if p = fst hd then
             if li = [] then let li = [snd (List.hd r)] in li @ (f rr tl p li) @ (f rr rr (snd (List.hd r)) li)
             else 
             if List.exists (fun x -> x = snd (List.hd r)) li then f rr tl p li
             else let lis = li @ [snd (List.hd r)] in [snd (List.hd r)] @ (f rr tl p lis) @ (f rr rr (snd (List.hd r)) lis)
           else f rr tl p li
  in let rec count r l = 
       match l with 
         [] -> 0
          |hd :: tl ->
            if List.exists (fun x -> x = hd) (f r r hd []) then 1 + count r tl
            else count r tl
  in count r (List.sort_uniq compare (ispossible r []))