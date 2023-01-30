open Ast
    
type loc = int

type envval =  IVar of loc  | IArr of loc * int | Procv of paramf * cmd

type memval = int

type env = ide -> envval
type mem = loc -> memval
type trm = Ok | Br
type exprval = Bool of bool | Int of int

(*il quarto componente dello stato è la prima locazione libera .*)
type state = env list * mem * trm * loc

let topenv (el,_,_,_) = match el with
    [] -> failwith "empty environment stack"
  | e::_ -> e

let popenv (el,_,_,_) = match el with
    [] -> failwith "empty environment stack"
  | _::el' -> el'

let getenv (el,_,_,_) = el
let getmem (_,m,_,_) = m
let gettrm (_,_,t,_) = t
let getloc (_,_,_,l) = l

  
type conf = St of state | Cmd of cmd * state

exception TypeError of string
exception UnboundVar of ide
exception UnboundLoc of loc
exception NoRuleApplies
exception BreakOutsideRepeat
exception IndexOutOfBound

