type bigint = BigInt of string
type op = ADD | MUL

let rec reverse s =
  let len = String.length s in
  if len = 1 then s :: []
  else reverse (String.sub s 1 (len-1)) @ (String.sub s 0 1 :: [])

let rec add x y =
  match x, y with 
  |[],_ |_,[] -> x @ y
  |hx :: tx, hy :: ty -> string_of_int(int_of_string(List.nth x 0)+int_of_string(List.nth y 0)) :: (add tx ty)

let rec mult x y = 
  match x with 
  |[] -> []
  |hd :: tl -> string_of_int(int_of_string(hd) * int_of_string(y)) :: mult tl y

let rec carry l =
  match l with 
  |[] -> []
  |hd :: tl -> if (int_of_string hd) < 10 then hd :: carry tl
      else 
      if tl = [] then carry(string_of_int(int_of_string(hd)-10) :: ["1"])
      else carry(string_of_int(int_of_string(hd)-10) :: carry (string_of_int(int_of_string(List.hd tl)+1) :: List.tl tl))

let rec make_string s =
  match s with
  |[] -> ""
  |hd :: tl -> make_string tl ^ hd

let rec string_to_list s = 
  match s with
  | "" -> []
  | s -> (String.sub s 0 1) :: (string_to_list (String.sub s 1 ((String.length s)-1)) )

let rec compute_bigint exp env =
  let big_int e =
    match e with
      BigInt e -> e
  in match exp with
  |ADD -> (match env with
      |(x,y) -> let xx = reverse (big_int x) in 
          let yy = reverse (big_int y) in 
          BigInt (make_string (carry(add xx yy))))
  |MUL -> (match env with 
      |(x,y) -> let xx = reverse (big_int x) in 
          let yy = reverse (big_int y) in
          let len_y = String.length (big_int y) in
          if len_y = 1 then BigInt (make_string (carry(mult xx (List.hd yy))))
          else BigInt (make_string (carry (add (carry (mult xx (List.hd yy))) (["0"]@(reverse (big_int(compute_bigint MUL (x, BigInt (make_string (List.tl yy)))))))))))