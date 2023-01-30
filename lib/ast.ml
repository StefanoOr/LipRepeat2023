type ide = string

type expr =
  | True
  | False
  | Var of ide 
  | Arr of ide * expr
  | Const of int
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Eq of expr * expr
  | Leq of expr * expr
  
(* dichiarazioni delle variabili *)
type declv =
  | EmptyDeclv
  | IntVar of ide
  | IntArr of ide * int
  | DvSeq of declv * declv
  
(* Parametri *)
type paramf = 
  | Val of ide
  | Ref of ide

type parama = expr
  
type cmd =
  | Skip
  | Break
  | Repeat of cmd 
  | RptSeq of cmd * cmd
  | Assign of ide * expr
  | ArrAssign of ide * expr * expr
  | Seq of cmd * cmd
  | If of expr * cmd * cmd
  | Block of declv * cmd
  | Call of ide * parama 
  

(* dichiarazioni delle procedure *)
type declp = Proc of ide * paramf * cmd

type declplist = DeclList of declp list

type prog = Prog of declv * declplist * cmd
