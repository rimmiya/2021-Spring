let rec iter n f x =
  match n with
  0 | 1 -> f(x)
  |n -> iter (n-1) f (f x);;