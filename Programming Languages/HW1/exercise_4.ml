type formula = TRUE | FALSE
  | NOT of formula
  | ANDALSO of formula * formula
  | ORELSE of formula * formula
  | IMPLY of formula * formula
  | LESS of expr * expr
and expr = NUM of int
  | PLUS of expr * expr
  | MINUS of expr * expr;;

let rec eval e =
  let rec expr e =
    match e with
    NUM n -> n
    |PLUS (n1, n2) -> (expr n1) + (expr n2)
    |MINUS (n1, n2) -> (expr n1) - (expr n2)
  in match e with
    TRUE -> true
    |FALSE -> false
    |NOT x -> not (eval x)
    |ANDALSO (x,y) -> (eval x) && (eval y)
    |ORELSE (x,y) -> (eval x) || (eval y)
    |IMPLY (x,y) -> (not (eval x)) || (eval y)
    |LESS (x, y) -> if (expr x) < (expr y) then true else false;;