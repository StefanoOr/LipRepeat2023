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
  | Assign of string * expr
  | Assign_arr of string * expr * expr
  | Seq of cmd * cmd
  | Repeat of cmd
  | If of expr * cmd * cmd
  | Block of cmd
  | Call of ide * expr

type decl_v =
  | NullVar
  | IntArr of ide * int
  | IntVar of ide
  | DVSeq of decl_v * decl_v

(*DICHIARAZIONE DI PROCEDURE*)
type decl_p =                       
  | NullProc                      
  | Proc of ide * par_f * cmd   
  | DPSeq of decl_p * decl_p

type prog = Prog of decl * cmd