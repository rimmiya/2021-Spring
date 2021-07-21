let rec range x y =
  if x = y then [x]
  else if x > y then []
  else x :: range (x+1) y;;