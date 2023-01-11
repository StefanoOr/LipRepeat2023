open Ast
open Types

let apply st x = match topenv st x with
    IVar l -> getmem st l
  | _ -> failwith "apply error"

let parse (s : string) : prog =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(******************************************************************************)
(*                      Small-step semantics of expressions                   *)
(******************************************************************************)

exception TypeError of string
exception UnboundVar of string
exception PredOfZero
exception NoRuleApplies

let botenv = fun x -> failwith ("variable " ^ x ^ " unbound")
let botmem = fun l -> failwith ("location " ^ string_of_int l ^ " undefined")
    
let bind f x v = fun y -> if y=x then v else f y

let is_val = function
    True -> true
  | False -> true
  | Const _ -> true
  | _ -> false

let rec trace1_expr st = function
  | Var x -> (Const(apply st x), st)
  | Not(True) -> (False,st)
  | Not(False) -> (True,st)
  | Not(e) -> let (e',st') = trace1_expr st e in (Not(e'),st')
  | And(True,e) -> (e,st)
  | And(False,_) -> (False,st)
  | And(e1,e2) -> let (e1',st') = trace1_expr st e1 in (And(e1',e2),st')
  | Or(True,_) -> (True,st)
  | Or(False,e) -> (e,st)
  | Or(e1,e2) -> let (e1',st') = trace1_expr st e1 in (Or(e1',e2),st')
  | Add(Const(n1),Const(n2)) -> (Const(n1+n2),st)
  | Add(Const(n1),e2) -> let (e2',st') = trace1_expr st e2 in (Add(Const(n1),e2'),st')
  | Add(e1,e2) -> let (e1',st') = trace1_expr st e1 in (Add(e1',e2),st')
  | Sub(Const(n1),Const(n2)) -> (Const(n1-n2),st)
  | Sub(Const(n1),e2) -> let (e2',st') = trace1_expr st e2 in (Sub(Const(n1),e2'),st')
  | Sub(e1,e2) -> let (e1',st') = trace1_expr st e1 in (Sub(e1',e2),st')
  | Mul(Const(n1),Const(n2)) -> (Const(n1*n2),st)
  | Mul(Const(n1),e2) -> let (e2',st') = trace1_expr st e2 in (Mul(Const(n1),e2'),st')
  | Mul(e1,e2) -> let (e1',st') = trace1_expr st e1 in (Mul(e1',e2),st')
  | Eq(Const(n1),Const(n2)) -> if n1=n2 then (True,st) else (False,st)
  | Eq(Const(n1),e2) -> let (e2',st') = trace1_expr st e2 in (Eq(Const(n1),e2'),st')
  | Eq(e1,e2) -> let (e1',st') = trace1_expr st e1 in (Eq(e1',e2),st')
  | Leq(Const(n1),Const(n2)) -> if n1<=n2 then (True,st) else (False,st)
  | Leq(Const(n1),e2) -> let (e2',st') = trace1_expr st e2 in (Leq(Const(n1),e2'),st')
  | Leq(e1,e2) -> let (e1',st') = trace1_expr st e1 in (Leq(e1',e2),st')
  

(* aggiungere call *)




and trace1_cmd = function
    St _ -> raise NoRuleApplies
  | Cmd(c,st) -> match c with
      Skip -> St st
    | Assign(x,Const(n)) -> (match topenv st x with
        IVar l -> St (getenv st, bind (getmem st) l n, getloc st)
      | _ -> failwith "improve err msg")
    | Assign(x,e) -> let (e',st') = trace1_expr st e in Cmd(Assign(x,e'),st') 
    | Seq(c1,c2) -> (match trace1_cmd (Cmd(c1,st)) with
          St st1 -> Cmd(c2,st1)
        | Cmd(c1',st1) -> Cmd(Seq(c1',c2),st1))
    | If(True,c1,_) -> Cmd(c1,st)
    | If(False,_,c2) -> Cmd(c2,st)
    | If(e,c1,c2) -> let (e',st') = trace1_expr st e in Cmd(If(e',c1,c2),st')

(* aggiungere il nostro while repeat *)




let rec sem_decl_v (e,l) = function
    EmptyDecl -> (e,l)
  | IntVar(x) ->  let e' = bind e x (IVar l) in (e',l+1)
  | DSeq_v(d1,d2) -> let (e',l') = sem_decl_v (e,l) d1 in sem_decl_v (e',l') d2

(* manca dichiarazione vettore *)
let rec sem_decl_p (e,l) = function
  EmptyDecl -> (e,l)
  | DSeq_p(d1,d2) -> let (e',l') = sem_decl_p (e,l) d1 in sem_decl_p (e',l') d2
  | Proc(f,x,c,er) -> let e' = bind e f (IFun(x,c,er)) in (e',l)




let rec trace_rec n t =
  if n<=0 then [t]
  else try
      let t' = trace1_cmd t
      in t::(trace_rec (n-1) t')
    with NoRuleApplies -> [t]


(**********************************************************************
 trace : int -> prog -> conf list
 Usage: trace n c performs n steps of the small-step semantics
 **********************************************************************)

let trace n (Prog(d,c)) =
  let (e,l) = sem_decl (botenv,0) d
  in trace_rec n (Cmd(c,([e],botmem,l)))
