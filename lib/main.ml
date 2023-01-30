open Ast
open Types

let parse (s : string) : prog =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(* empty env *)
let botenv = fun x -> raise (UnboundVar x)
(* empty mem *)
let botmem = fun l -> raise (UnboundLoc l)

(* apply : state -> ide -> memval *)
(* returns the memory value of the variable *)
let apply st x = match topenv st x with
  IVar l -> getmem st l
| _ -> failwith "apply error"

(* apply_array : state -> ide -> int -> memval *)
(* returns the memval of an array named x.
 * index is summed with the location of the first element *)
let apply_array st x index = 
    match (topenv st) x with
  | IArr (l, dim) -> 
          if(index >= 0 && index < dim) 
          then (getmem st) (l+index) 
          else raise IndexOutOfBound
  | _ -> failwith "apply_array applies only to arrays"

(* int_of_exprval : exprval -> int 
 * Checks if an exprval is an int *)
let int_of_exprval = function
    Int n -> n
  | _ -> raise (TypeError "exprval must be an int")

(* eval_expr : state -> expr -> exprval *)
(* Big-step semantics of expressions *)
let rec eval_expr st = function
    True -> Bool true
  | False -> Bool false 
  | Var x ->  Int (apply st x) (* memory value of x *)
  | Arr(x,e) -> 
      let i = int_of_exprval (eval_expr st e) in 
      Int(apply_array st x i) (* memory value of x[i] *)
  | Const n -> Int n
  | Not(e) -> (match eval_expr st e with
        Bool b -> Bool(not b)
      | _ -> raise (TypeError "Not")
      )
  | And(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Bool b1,Bool b2) -> Bool(b1 && b2)
      | _ -> raise (TypeError "And")
      )
  | Or(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Bool b1,Bool b2) -> Bool(b1 || b2)
      | _ -> raise (TypeError "Or")
      )
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
  | Eq(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Int n1,Int n2) -> Bool(n1 = n2)
      | _ -> raise (TypeError "Eq")
      )    
  | Leq(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Int n1,Int n2) -> Bool(n1 <= n2)
      | _ -> raise (TypeError "Leq")
      )        

(* bind : ('a -> 'b) -> 'c -> 'b -> ('a -> 'b) *)
(* Defines a new association from x to v for the function f. *)
let bind f x v = fun y -> if y=x then v else f y

(* sem_declv : env * loc -> env * loc *)
(* Big-step semantics of variable declarations. 
 * Returns the couple (environment, location). *)
let rec sem_declv (r,l) = function
    EmptyDeclv -> (r,l)
  | IntVar x -> let r' = bind r x (IVar l) in (r',l+1)
  | IntArr(x,dim) -> let r' = bind r x (IArr(l,dim)) in (r',l+dim)
  | DvSeq(dv1,dv2) -> let (r',l') = sem_declv (r,l) dv1 in sem_declv (r',l') dv2

(* sem_declp : env -> declplist -> env *)
(* Big-step semantics of procedure declarations
 * Returns the new environment. *)
let rec sem_declp r (DeclList l) = match l with
    [] -> r
  | Proc(x,pf,c)::dpl -> let r' = bind r x (Procv(pf,c)) in sem_declp r' (DeclList dpl)

(* trace1 : conf -> conf *)
(* Small-step semantics of configurations. 
 * Implements the inference rules in README.md *)
let rec trace1 = function
    (* If there is a break then it is outside a repeat forever, else the program has terminated. *)
    St st when (gettrm st) = Br -> raise BreakOutsideRepeat
  | St _ -> raise NoRuleApplies
  | Cmd(_,st) when (gettrm st) = Br -> raise BreakOutsideRepeat
  | Cmd(c,st) -> match c with
          Skip -> St st (* Returning the current state *)
        | Break -> St (getenv st, getmem st, Br, getloc st) (* Setting the Br tag *)
        | Repeat c -> Cmd((RptSeq(c,c)),st) (* Entering in the repeat *)
        | RptSeq(c',c) -> (match trace1 (Cmd(c',st)) with (* Reducing the current command (c'), c is the initial one *)
              (* Exiting the repeat if c' is a break *)
              St st' 
            | Cmd(_,st') when (gettrm st') = Br -> St (getenv st', getmem st', Ok, getloc st') 
            | St st' -> Cmd((RptSeq(c,c)),st') (* Repeating the initial command *)
            | Cmd(c'',st') -> Cmd((RptSeq(c'',c)),st'))
        | Assign(x,e) -> 
            let v = int_of_exprval (eval_expr st e) in (* Value to be saved *)
            (match (topenv st) x with (* Memory location retrieval *)
               (* Saving in memory *)
               IVar l -> St (getenv st, bind (getmem st) l v, Ok, getloc st) 
             | _ -> raise NoRuleApplies)
        | Assign_arr(x,e1,e2) -> 
            let i = int_of_exprval (eval_expr st e1) in (* index *)
            let v = int_of_exprval (eval_expr st e2) in (* value *)
            (match (topenv st) x with
               IArr(l,dim) ->
                 if i >= 0 && i < dim 
                 (* Saving in memory *)
                 then St (getenv st, bind (getmem st) (l+i) v, Ok, getloc st)
                 else raise IndexOutOfBound
             | _ -> raise NoRuleApplies)
        | Seq(c1,c2) -> (match trace1 (Cmd(c1,st)) with
            St st' -> Cmd(c2,st') 
          | Cmd(c1',st') -> Cmd(Seq(c1',c2),st'))
        | If(e,c1,c2) -> (match eval_expr st e with
            Bool true -> Cmd(c1,st)
          | Bool false -> Cmd(c2,st)
          | _ -> raise (TypeError "expr in If must be a bool"))
        (* Declarations were already decleared *)
        | Block(EmptyDeclv,c1) -> (match trace1 (Cmd(c1,st)) with
            St st' -> St(popenv st', getmem st', Ok, getloc st') (* Returning the state without the local environment *)
          | Cmd(c',st') -> (Cmd(Block(EmptyDeclv,c'),st'))) 
        | Block(dv,c) -> 
            (* Declaring variables *)
            let (r,l) = sem_declv (topenv st, getloc st) dv in
            (* New environment on the top of the stack (local + non local block env) *)
            let st' = (r::(getenv st), getmem st, Ok, l) in
            (Cmd(Block(EmptyDeclv,c),st')) (* The block is ready to be executed *)
        | Call(x,p) ->
            let r = topenv st in
            match r x with
                Procv(Val y,c) ->
                  let v = eval_expr st p in (* Value of the actual param p *)
                  let l = getloc st in    (* First free location *)
                  (* Binding the formal param y to l *)
                  let r' = bind r y (IVar l) in
                  (* Saving the actual param v in l *)
                  let s = bind (getmem st) l (int_of_exprval v) in 
                  (* Wrapping the procedure command with a block
                     (updating the first available memory location as it is associated with y) *)
                  trace1 (Cmd(Block(EmptyDeclv,c), (r'::getenv st, s, Ok, l+1)))
              | Procv(Ref y,c) -> (match p with (* Checking if the reference parameter is 
                                                   a variable or an array and acts accordingly *)
                  Var x -> 
                    (* Binding the formal param y to the memory location of x *)
                    let r' = bind r y (r x) in
                    (* Wrapping the procedure command with a block *)
                    trace1 (Cmd(Block(EmptyDeclv,c), (r'::getenv st, getmem st, Ok, (getloc st))))
                | Arr(x,e) -> 
                    let i = int_of_exprval (eval_expr st e) in (* index *)
                    (match (r x) with
                       IArr(l,d) -> 
                         if i >= 0 && i < d
                         then 
                           (* Binding the formal param y to the memory location of x[i] *)
                           let r' = bind r y (IVar (l+i)) in
                           trace1 (Cmd(Block(EmptyDeclv,c), (r'::getenv st, getmem st, Ok, (getloc st))))
                         else raise IndexOutOfBound
                    | _ -> raise NoRuleApplies)
                | _ -> failwith "Procedures take only variables or array cells as ref")
              | _ -> failwith (x ^ " is not a procedure")

(* trace_rec : int -> conf -> conf list*)
let rec trace_rec n conf =
    if n<=0 then [conf]
    else try
        let conf' = trace1 conf
        in conf::(trace_rec (n-1) conf')
    with NoRuleApplies -> [conf]

(* trace : int -> prog -> conf list *)
let trace n (Prog(dv,dpl,c)) = 
    let (r,l) = sem_declv (botenv,0) dv in 
    let r' = sem_declp r dpl in
    trace_rec n (Cmd(c,([r'],botmem,Ok,l)))
