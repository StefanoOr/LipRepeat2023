open Ast
open Types

let parse (s : string) : prog =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(*  env vuoto *)
let botenv = fun x -> raise (UnboundVar x)
(*  mem vuoto *)
let botmem = fun l -> raise (UnboundLoc l)

(* apply : state -> ide -> memval *)
(*  ritorna il valore della memoria della variabile  *)
let apply st x = match topenv st x with
  IVar l -> getmem st l
| _ -> failwith "apply error"

(* apply_array : state -> ide -> int -> memval *)
(*  restituisce  il memval dell'array x
 *  indice è sommato con alla locazione del primo elemento *)
let apply_array st x index = 
    match (topenv st) x with
  | IArr (l, dim) -> 
          if(index >= 0 && index < dim) 
          then (getmem st) (l+index) 
          else raise IndexOutOfBound
  | _ -> failwith "apply_array si applica solo agli array"

(* int_of_exprval : exprval -> int 
 *  controlla se un exprval è un intero *)
let int_of_exprval = function
    Int n -> n
  | _ -> raise (TypeError "exprval deve essere un intero")

(* eval_expr : state -> expr -> exprval *)
(* Big-step semantics of expressions *)
let rec eval_expr st = function
  Const n -> Int n  
  | Add(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Int n1,Int n2) -> Int(n1 + n2)
      | _ -> raise (TypeError "Add")
      )    
  | Sub(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Int n1,Int n2) when n1>=n2 -> Int(n1 - n2)
      | _ -> raise (TypeError "Sub")
      )
  | Mul(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Int n1,Int n2) -> Int(n1 * n2)
      | _ -> raise (TypeError "Add")
      )
  | True -> Bool true
  | False -> Bool false 
  | And(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Bool b1,Bool b2) -> Bool(b1 && b2)
      | _ -> raise (TypeError "And")
      )
  | Or(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Bool b1,Bool b2) -> Bool(b1 || b2)
      | _ -> raise (TypeError "Or")
      )
  | Not(e) -> (match eval_expr st e with
        Bool b -> Bool(not b)
      | _ -> raise (TypeError "Not")
      )        
  | Eq(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Int n1,Int n2) -> Bool(n1 = n2)
      | _ -> raise (TypeError "Eq")
      )    
  | Leq(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Int n1,Int n2) -> Bool(n1 <= n2)
      | _ -> raise (TypeError "Leq")
      )
  | Var x ->  Int (apply st x) (*valore della memoria x *)
  | Arr(x,e) -> 
      let i = int_of_exprval (eval_expr st e) in 
      Int(apply_array st x i) (*valore della memoria x [i] *)

(* bind : ('a -> 'b) -> 'c -> 'b -> ('a -> 'b) *)
(*   Definisce una nuova assocciazione da x a v per  la funzione f*)
let bind f x v = fun y -> if y=x then v else f y

(* sem_decl_v : env * loc -> env * loc *)
(* Big-step semantics of variable declarations.  
 * restituisce la coppia (environment, location). *)
let rec sem_decl_v (r,l) = function
    NullVar -> (r,l)
  | IntVar x -> let r' = bind r x (IVar l) in (r',l+1)
  | IntArr(x,dim) -> let r' = bind r x (IArr(l,dim)) in (r',l+dim)
  | DvSeq(dv1,dv2) -> let (r',l') = sem_decl_v (r,l) dv1 in sem_decl_v (r',l') dv2

(* sem_decl_p : env -> declplist -> env *)
(* Returns the new environment. restituisce il nuovo ambiente  *)
let rec sem_decl_p r (DeclList l) = match l with
    [] -> r
  | Proc(x,pf,c)::dpl -> let r' = bind r x (Procv(pf,c)) in sem_decl_p r' (DeclList dpl)

(* trace1 : conf -> conf *)

let rec trace1 = function
    (* Se c'e un break all'interno di una repeat esce dalla repeat , se fuori termina il programma  *)
    St st when (gettrm st) = Br -> raise BreakOutsideRepeat
  | St _ -> raise NoRuleApplies
  | Cmd(_,st) when (gettrm st) = Br -> raise BreakOutsideRepeat
  | Cmd(c,st) -> match c with
          Skip -> St st (* restituisce lo stato corrente  *)
        | Break -> St (getenv st, getmem st, Br, getloc st)
        | Assign(x,e) -> 
          let v = int_of_exprval (eval_expr st e) in (*valore da salvare  *)
          (match (topenv st) x with (* recupera locazione di memoria  *)
             (*salva in memoria  *)
             IVar l -> St (getenv st, bind (getmem st) l v, Ok, getloc st) 
           | _ -> raise NoRuleApplies)
        | Assign_arr(x,e1,e2) -> 
            let i = int_of_exprval (eval_expr st e1) in (* index *)
            let v = int_of_exprval (eval_expr st e2) in (* value *)
            (match (topenv st) x with
              IArr(l,dim) ->
                if i >= 0 && i < dim 
                (* salva in memoria  *)
                then St (getenv st, bind (getmem st) (l+i) v, Ok, getloc st)
                else raise IndexOutOfBound
            | _ -> raise NoRuleApplies)
        | Seq(c1,c2) -> (match trace1 (Cmd(c1,st)) with
            St st' -> Cmd(c2,st') 
          | Cmd(c1',st') -> Cmd(Seq(c1',c2),st')) 
          | Repeat c -> Cmd((RptSeq(c,c)),st) (* entra nel  repeat *)
          | RptSeq(c',c) -> (match trace1 (Cmd(c',st)) with (*  riduuce il commando corrente *)
                (* esce dal repeat se c' è un break *)
                St st' 
              | Cmd(_,st') when (gettrm st') = Br -> St (getenv st', getmem st', Ok, getloc st') 
              | St st' -> Cmd((RptSeq(c,c)),st') 
              | Cmd(c'',st') -> Cmd((RptSeq(c'',c)),st'))
        | If(e,c1,c2) -> (match eval_expr st e with
            Bool true -> Cmd(c1,st)
          | Bool false -> Cmd(c2,st)
          | _ -> raise (TypeError "L'espressione valutata dall' IF deve avere tipo bool"))       
        | Block(NullVar,c1) -> (match trace1 (Cmd(c1,st)) with
            St st' -> St(popenv st', getmem st', Ok, getloc st') (* ritorna lo stato senza l'ambiente locale *)
          | Cmd(c',st') -> (Cmd(Block(NullVar,c'),st'))) 
        | Block(dv,c) -> 
            (* dichiarazione variabile  *)
            let (r,l) = sem_decl_v (topenv st, getloc st) dv in
            (*    nuovo ambiente in cima allo stack *)
            let st' = (r::(getenv st), getmem st, Ok, l) in
            (Cmd(Block(NullVar,c),st')) (*il blocco è pronto per essere eseguito *)
        | Call(x,p) ->
            let r = topenv st in
            match r x with
                Procv(Val y,c) ->
                  let v = eval_expr st p in (*  valore dell'attuale parametro p *)
                  let l = getloc st in    (* prima locazione libera  *)
                  (* associa il parametro y a l *)
                  let r' = bind r y (IVar l) in
                  (*salva l'attuale parametro v in l  *)
                  let s = bind (getmem st) l (int_of_exprval v) in 
                
                  trace1 (Cmd(Block(NullVar,c), (r'::getenv st, s, Ok, l+1)))
              | Procv(Ref y,c) -> (match p with (* controlla se il parametro referente è una variabile o un array*)
                  Var x ->   
                    let r' = bind r y (r x) in
                    trace1 (Cmd(Block(NullVar,c), (r'::getenv st, getmem st, Ok, (getloc st))))
                | Arr(x,e) -> 
                    let i = int_of_exprval (eval_expr st e) in (* index *)
                    (match (r x) with
                       IArr(l,d) -> 
                         if i >= 0 && i < d
                         then 
                           let r' = bind r y (IVar (l+i)) in
                           trace1 (Cmd(Block(NullVar,c), (r'::getenv st, getmem st, Ok, (getloc st))))
                         else raise IndexOutOfBound
                    | _ -> raise NoRuleApplies)
                | _ -> failwith "Una procedura prende solo una variabile o la cella di un array come riferimento")
              | _ -> failwith (x ^ " non è una procedura")

(* trace_rec : int -> conf -> conf list*)
let rec trace_rec n conf =
    if n<=0 then [conf]
    else try
        let conf' = trace1 conf
        in conf::(trace_rec (n-1) conf')
    with NoRuleApplies -> [conf]

(* trace : int -> prog -> conf list *)
let trace n (Prog(dv,dpl,c)) = 
    let (r,l) = sem_decl_v (botenv,0) dv in 
    let r' = sem_decl_p r dpl in
    trace_rec n (Cmd(c,([r'],botmem,Ok,l)))
