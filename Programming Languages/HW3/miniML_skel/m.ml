type program = exp
and exp = 
  | CONST of int
  | VAR of var
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | ISZERO of exp
  | READ
  | IF of exp * exp * exp
  | LET of var * exp * exp
  | LETREC of var * var * exp * exp
  | PROC of var * exp
  | CALL of exp * exp
  | NEWREF of exp 
  | DEREF of exp
  | SETREF of exp * exp
  | SEQ of exp * exp
  | BEGIN of exp
and var = string

type value = 
    Int of int 
  | Bool of bool 
  | Procedure of var * exp * env 
  | RecProcedure of var * var * exp * env
  | Loc of loc
and loc = int
and env = (var * value) list
and mem = (loc * value) list

(* conversion of value to string *)
let value2str v = 
  match v with
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | Loc l -> "Loc "^(string_of_int l)
  | Procedure (x,e,env) -> "Procedure "
  | RecProcedure (f,x,e,env) -> "RecProcedure "^f

(* environment *)
let empty_env = []
let extend_env (x,v) e = (x,v)::e
let rec apply_env e x = 
  match e with
  | [] -> raise (Failure (x ^ " is unbound in Env"))
  | (y,v)::tl -> if x = y then v else apply_env tl x

(* memory *)
let empty_mem = [] 
let extend_mem (l,v) m = (l,v)::m
let rec apply_mem m l = 
  match m with
  | [] -> raise (Failure ("Location " ^ string_of_int l ^ " is unbound in Mem"))
  | (y,v)::tl -> if l = y then v else apply_mem tl l

(* use the function 'new_location' to generate a fresh memory location *)
let counter = ref 0
let new_location () = counter:=!counter+1;!counter

exception NotImplemented
exception UndefinedSemantics

(*****************************************************************)
(* TODO: Implement the eval function. Modify this function only. *)
(*****************************************************************)
let rec eval : exp -> env -> mem -> value * mem
=fun exp env mem ->
  match exp with
  |CONST n -> (Int n, mem)
  |VAR x -> (apply_env env x, mem)
  |ADD (e1, e2) -> let (n1, m1) = eval e1 env mem in
                    let (n2, m2) = eval e2 env m1 in
                      (match (n1, n2) with
                      |(Int n1, Int n2) -> let n = Int(n1+n2) in (n, m2)
                      |_ -> raise UndefinedSemantics)
  |SUB (e1, e2) -> let (n1, m1) = eval e1 env mem in
                    let (n2, m2) = eval e2 env m1 in
                      (match (n1, n2) with
                      |(Int n1, Int n2) -> let n = Int(n1-n2) in (n, m2)
                      |_ -> raise UndefinedSemantics)
  |MUL (e1, e2) -> let (n1, m1) = eval e1 env mem in
                    let (n2, m2) = eval e2 env m1 in
                      (match (n1, n2) with
                      |(Int n1, Int n2) -> let n = Int(n1*n2) in (n, m2)
                      |_ -> raise UndefinedSemantics)
  |DIV (e1, e2) -> let (n1, m1) = eval e1 env mem in
                    let (n2, m2) = eval e2 env m1 in
                      (match (n1, n2) with
                      |(Int n1, Int n2) -> let n = Int(n1/n2) in (n, m2)
                      |_ -> raise UndefinedSemantics)
  |ISZERO e -> let (n, m) = eval e env mem in
                  (match n with
                  |Int n -> if n = 0 then (Bool true, m) else (Bool false, m)
                  |_ -> raise UndefinedSemantics)
  |READ -> (Int (read_int()), mem)
  |IF (e1, e2, e3) -> let (tf, m1) = eval e1 env mem in
                        (match tf with
                        |Bool true -> eval e2 env m1
                        |Bool false -> eval e3 env m1
                        |_ -> raise UndefinedSemantics)
  |LET (x, e1, e2) -> let (v1, m1) = eval e1 env mem in
                       let env1 = extend_env (x, v1) env in
                        eval e2 env1 m1
  |LETREC (f, x, e1, e2) -> let v = RecProcedure (f, x, e1, env) in
                             let env1 = extend_env (f, v) env in
                              eval e2 env1 mem
  |PROC (x, e) -> (Procedure (x, e, env), mem)
  |CALL (e1, e2) -> let (n, m1) = eval e1 env mem in
                     let (v, m2) = eval e2 env m1 in
                      (match n with
                      |Procedure (x, e, p) -> eval e (extend_env (x, v) p) m2
                      |RecProcedure (f, x, e, p) -> eval e (extend_env (x, v) (extend_env (f, RecProcedure(f, x, e, p)) p)) m2
                      |_ -> raise UndefinedSemantics)
  |NEWREF e -> let (v, m) = eval e env mem in
                let l = new_location() in
                 let m1 = extend_mem (l, v) m in 
                  (Loc l, m1)
  |DEREF e -> let (l, m) = eval e env mem in
                (match l with
                |Loc l -> (apply_mem m l, m)
                |_ -> raise UndefinedSemantics)
  |SETREF (e1, e2) -> let (l, m1) = eval e1 env mem in
                        let (v, m2) = eval e2 env m1 in
                          (match l with
                          |Loc l -> let m3 = extend_mem (l, v) m2 in (v, m3)
                          |_ -> raise UndefinedSemantics)
  |SEQ (e1, e2) -> let (n1, m1) = eval e1 env mem in
                      eval e2 env m1
  |BEGIN e -> eval e env mem

(* driver code *)
let run : program -> value
=fun pgm -> (fun (v,_) -> v) (eval pgm empty_env empty_mem) 
