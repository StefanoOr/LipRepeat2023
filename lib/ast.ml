type ide = string

type expr =
  | Const of int
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | True
  | False
  | And of expr * expr
  | Or of expr * expr
  | Not of expr  
  | Eq of expr * expr
  | Leq of expr * expr
  | Var of ide 
  | Arr of ide * expr
  
(* dichiarazioni delle variabili *)
type decl_v =
  | NullVar
  | IntVar of ide
  | IntArr of ide * int
  | DvSeq of decl_v * decl_v
  
(* Parametri *)
type paramf = 
  | Val of ide
  | Ref of ide

type parama = expr
  
type cmd =
  | Skip
  | Break
  | Assign of ide * expr
  | Assign_arr of ide * expr * expr
  | Seq of cmd * cmd
  | Repeat of cmd 
  | RptSeq of cmd * cmd
  | If of expr * cmd * cmd
  | Block of decl_v * cmd
  | Call of ide * parama 
  

(* dichiarazioni delle procedure *)
type decl_p = Proc of ide * paramf * cmd

type declplist = DeclList of decl_p list

type prog = Prog of decl_v * declplist * cmd
