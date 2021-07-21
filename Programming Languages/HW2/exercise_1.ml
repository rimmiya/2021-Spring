type exp = X | INT of int
  | REAL of float
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp
  | INTEGRAL of exp * exp * exp

exception FreeVariable

let rec calc exp env =
    let rec sigma (n, m, f) =
        if n <= m then (calc f n) +. sigma (n +. 1.0, m, f)
        else 0.
    in let rec integral (n, m, f)=
        if n <= m then integral ((n +. 0.1), m, f) +. (calc f n) *. 0.1
        else 0.
    in match exp with
    |X -> env
    |INT x -> float_of_int x
    |REAL x -> x
    |ADD (x1, x2) -> (calc x1 env) +. (calc x2 env)
    |SUB (x1, x2) -> (calc x1 env) -. (calc x2 env)
    |MUL (x1, x2) -> (calc x1 env) *. (calc x2 env)
    |DIV (x1, x2) -> (calc x1 env) /. (calc x2 env)
    |SIGMA (n, m, f) -> sigma ((calc n env), (calc m env), f)
    |INTEGRAL (n, m, f) -> integral ((calc n env), (calc m env) -. 0.1, f)


let rec calculate e =
    let rec sigma (n, m, f) =
        if n <= m then (calc f n) +. sigma (n +. 1.0, m, f)
        else 0.
    in let rec integral (n, m, f)=
        if n <= m then integral ((n +. 0.1), m, f) +. (calc f n) *. 0.1
        else 0.
  in match e with
  |X -> raise FreeVariable
  |INT x -> float_of_int x
  |REAL x -> x
  |ADD (x1, x2) -> (calculate x1) +. (calculate x2)
  |SUB (x1, x2) -> (calculate x1) -. (calculate x2)
  |MUL (x1, x2) -> (calculate x1) *. (calculate x2)
  |DIV (x1, x2) -> (calculate x1) /. (calculate x2)
  |SIGMA (n, m, f) -> sigma (calculate n, calculate m , f)
  |INTEGRAL (n, m, f) -> integral ((calculate n), (calculate m) -. 0.1, f)
