open Ast
open Types
    
(* Only integers are saved in memory *)
let string_of_val i = string_of_int i

let rec string_of_expr = function
    True -> "true"
  | False -> "false"
  | Var x -> x
  | Arr(x,e) -> x ^ "[" ^ string_of_expr e ^ "]" 
  | Const n -> string_of_int n
  | Not e -> "not " ^ string_of_expr e
  | And(e1,e2) -> string_of_expr e1 ^ " and " ^ string_of_expr e2
  | Or(e1,e2) -> string_of_expr e1 ^ " or " ^ string_of_expr e2
  | Add(e1,e2) -> string_of_expr e1 ^ "+" ^ string_of_expr e2
  | Sub(e1,e2) -> string_of_expr e1 ^ "-" ^ string_of_expr e2
  | Mul(e1,e2) -> string_of_expr e1 ^ "*" ^ string_of_expr e2
  | Eq(e1,e2) -> string_of_expr e1 ^ "=" ^ string_of_expr e2
  | Leq(e1,e2) -> string_of_expr e1 ^ "<=" ^ string_of_expr e2

let string_of_paramf = function
  | Val ide -> "val " ^ ide
  | Ref ide -> "ref" ^ ide

let string_of_parama = string_of_expr 

let rec string_of_declv = function
  | EmptyDeclv -> ""
  | DvSeq(d1, d2) -> string_of_declv d1 ^ " ; " ^ string_of_declv d2
  | IntVar x -> x 
  | IntArr(x,dim) -> x ^ "[" ^ string_of_int dim ^ "]"

let rec string_of_cmd = function
    Skip -> "skip"
  | Break -> "break"
  | Assign(x,e) -> x ^ ":=" ^ string_of_expr e
  | ArrAssign(x,e1,e2) -> x ^ "[" ^ string_of_expr e1 ^ "]:=" ^ string_of_expr e2
  | Seq(c1,c2) -> string_of_cmd c1 ^ "; " ^ string_of_cmd c2
  | If(e,c1,c2) -> "if " ^ string_of_expr e ^ " then " ^ string_of_cmd c1 ^ " else " ^ string_of_cmd c2
  | Repeat c -> "repeat " ^ string_of_cmd c ^ " forever"
  | Block(d,c) -> string_of_declv d ^ " { " ^ string_of_cmd c ^ " }"
  | Call(ide, param) -> ide ^ "(" ^ string_of_parama param ^ ")"
  | RptSeq(c1,c2)-> "rptseq( " ^ string_of_cmd c1 ^ " , " ^ string_of_cmd c2 ^ " )"

let string_of_declp = function
    Proc(ide,param,c) -> "proc " ^ ide ^ "(" ^ string_of_paramf param ^ ") {" ^ string_of_cmd c ^ "}"

let rec string_of_declplist = function
    DeclList [] -> ""
  | DeclList (d::t) -> string_of_declp d ^ string_of_declplist (DeclList t)

let string_of_prog = function
  Prog(dv,dp,c) -> string_of_declv dv ^ string_of_declplist dp ^ string_of_cmd c 


let string_of_loc f = "loc: " ^ string_of_int f

let string_of_envval = function
    IVar l -> "IVal( " ^ string_of_loc l ^ " ) "
  | IArr(l,dim) -> "IArr( " ^ string_of_loc l ^ ", " ^ string_of_int dim ^ " ) "
  | Procv(p,c) -> "proc( " ^ string_of_paramf p ^ ") { " ^ string_of_cmd c ^ " } "

let string_of_memval f = "memval: " ^ string_of_int f
  

let string_of_env1 s x = match topenv s x with
  | IVar l -> string_of_int l ^ "/" ^ x
  | IArr(l,_) -> string_of_int l ^ "/" ^ x
  | Procv(p,_) -> string_of_paramf p ^ "/" ^ x

let rec string_of_env s = function
    [] -> ""
  | [x] -> (try string_of_env1 s x with _ -> "")
  | x::dom' -> (try string_of_env1 s x ^ "," ^ string_of_env s dom'
                with _ -> string_of_env s dom')

let string_of_mem1 (m,l) i =
  assert (i<l);
  string_of_memval (m i) ^ "/" ^ string_of_int i

let rec range a b = if b<a then [] else a::(range (a+1) b);;

let string_of_mem (m,l) =
  List.fold_left (fun str i -> str ^ (try string_of_mem1 (m,l) i ^ "," with _ -> "")) "" (range 0 (l - 1))

let rec getlocs e = function
    [] -> []
  | x::dom -> try (match e x with
    | IVar l -> l::(getlocs e dom)
    | IArr(l,dim) -> (l+dim)::(getlocs e dom) (* first location of the array plus the index *)
    | Procv(_,_) -> (getlocs e dom))
    with _ -> getlocs e dom
                   
let string_of_state st dom =
  "[" ^ string_of_env st dom ^ "], " ^
  "[" ^ string_of_mem (getmem st,getloc st) ^ "]" ^ ", " ^
  string_of_int (getloc st)

let rec union l1 l2 = match l1 with
    [] -> l2
  | x::l1' -> (if List.mem x l2 then [] else [x]) @ union l1' l2

let rec vars_of_expr = function
    True
  | False
  | Const _ -> []
  | Var x -> [x]
  | Arr(x,_) -> [x]  
  | Not e -> vars_of_expr e
  | And(e1,e2) 
  | Or(e1,e2) 
  | Add(e1,e2)
  | Sub(e1,e2)
  | Mul(e1,e2)      
  | Eq(e1,e2) 
  | Leq(e1,e2) -> union (vars_of_expr e1) (vars_of_expr e2)

let rec vars_of_cmd = function
    Skip
  | Break -> []
  | Assign(x,e) -> union [x] (vars_of_expr e)
  | ArrAssign(x,e1,e2) -> union (union [x] (vars_of_expr e1)) (vars_of_expr e2)
  | Seq(c1,c2) -> union (vars_of_cmd c1) (vars_of_cmd c2)
  | If(e,c1,c2) -> union (vars_of_expr e) (union (vars_of_cmd c1) (vars_of_cmd c2))
  | Repeat c -> vars_of_cmd c
  | Block(_,c) -> vars_of_cmd c
  | Call(_,_) -> []
  | RptSeq (c0,_) -> vars_of_cmd c0

let vars_of_prog = function
    Prog(_,_,c) -> vars_of_cmd c 

let string_of_conf vars = function
    St st -> string_of_state st vars
  | Cmd(c,st) -> "<" ^ string_of_cmd c ^ ", " ^ string_of_state st vars ^ ">"

let rec string_of_trace vars = function
    [] -> ""
  | [x] -> (string_of_conf vars x)
  | x::l -> (string_of_conf vars x) ^ "\n -> " ^ string_of_trace vars l

let rec last = function
    [] -> failwith "last on empty list"
  | [x] -> x
  | _::l -> last l
