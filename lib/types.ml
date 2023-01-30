open Ast
    
type loc = int

type envval = 
    IVar of loc 
  | IArr of loc * int
  | Procv of paramf * cmd

type memval = int

type env = ide -> envval
type mem = loc -> memval
type trm = Ok | Br
type exprval = Bool of bool | Int of int

(* The third component of the state is the first free location.
   We assume that the store is unbounded *)
type state = env list * mem * trm * loc

let topenv (el,_,_,_) = match el with
    [] -> failwith "empty environment stack"
  | e::_ -> e

let popenv (el,_,_,_) = match el with
    [] -> failwith "empty environment stack"
  | _::el' -> el'

let getenv (el,_,_,_) = el
let getmem (_,m,_,_) = m
let getloc (_,_,_,l) = l
let gettrm (_,_,t,_) = t
  
type conf = St of state | Cmd of cmd * state

exception TypeError of string
exception UnboundVar of ide
exception UnboundLoc of loc
exception NoRuleApplies
exception BreakOutsideRepeat
exception IndexOutOfBound

