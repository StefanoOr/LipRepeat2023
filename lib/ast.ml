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
  | Array of ide * expr

type cmd =
  | Skip
  | Break
  | Assign of ide * expr
  | Assign_arr of ide * expr * expr
  | Seq of cmd * cmd
  | Repeat of cmd
  | Repeat_seq of cmd * cmd
  | If of expr * cmd * cmd
  | Block of declv * cmd
  | Call of ide * param

type declv =
  | EmptyDecl
  | IntArr of ide * int
  | IntVar of ide
  | DVSeq of declv * declv
  
type paramf = 
  | Val of ide
  | Ref of ide

type parama = expr

(*DICHIARAZIONE DI PROCEDURE*)
type declp = Proc of ide * paramf * cmd

type declplist = DeclList of declp list

type prog = Prog of declv * declplist * cmd
