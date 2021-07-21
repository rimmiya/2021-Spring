type ae = CONST of int
  | VAR of string
  | POWER of string * int
  | TIMES of ae list
  | SUM of ae list

let rec diff (ae, s) =
  match ae with
  |CONST x -> CONST 0
  |VAR v -> if v = s then CONST 1 else CONST 0
  |POWER (v, x) -> if v = s then TIMES[CONST x; POWER (v, x-1)] else CONST 0
  |TIMES l -> (match l with
              [] -> CONST 0
              |hd :: tl -> SUM[TIMES (diff (hd, s) :: tl); TIMES[hd; diff (TIMES tl, s)]])
  |SUM l -> (match l with
            [] -> CONST 0
            |hd :: tl -> SUM[diff(hd, s); diff(SUM tl, s)])