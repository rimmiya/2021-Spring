let rec merge x y = 
  match x with
  [] -> y
  |hx :: tx -> match y with
    [] -> x
    |hy :: ty -> if hx > hy then hx :: merge tx y else hy :: merge x ty;;