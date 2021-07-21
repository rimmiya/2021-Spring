let rec gcd n m =
  if m = 0 then n
  else if m > n then gcd (m - n) n
  else gcd (n - m) m;;