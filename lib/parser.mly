%{
open Ast
%}

%token TRUE
%token FALSE
%token NOT
%token AND
%token OR
%token ADD
%token SUB
%token MUL
%token EQ
%token LEQ
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token RBRACKET
%token LBRACKET
%token EOF
%token <string> ID
%token <int> CONST
%token SKIP
%token BREAK
%token ASSIGN
%token ARRAY
%token SEQ
%token IF
%token THEN
%token ELSE
%token REPEAT
%token FOREVER
%token PROC
%token REF
%token VAL
%token INTDEC

%left SEQ
%nonassoc ELSE 
%left OR
%left AND
%nonassoc NOT
%left EQ LEQ
%left ADD SUB
%left MUL

%start <prog> prog (* takes a program which is a command *)

%%

prog:
  | dv=decl_v; dp=declplist; c=cmd; EOF { Prog(dv,dp,c) }
;

expr:
  | x=CONST; { Const x }
  | e1=expr; ADD; e2=expr; { Add(e1,e2) }
  | e1=expr; SUB; e2=expr; { Sub(e1,e2) }
  | e1=expr; MUL; e2=expr; { Mul(e1,e2) }
  | TRUE { True }
  | FALSE { False }
  | NOT; e=expr; { Not(e) }
  | e1=expr; AND; e2=expr; { And(e1,e2) }
  | e1=expr; OR; e2=expr; { Or(e1,e2) }
  | e1=expr; EQ; e2=expr; { Eq(e1,e2) }
  | e1=expr; LEQ; e2=expr; { Leq(e1,e2) }
  | x=ID; { Var x }
  | x=ID; LBRACKET; e=expr; RBRACKET; { Arr(x,e) }
  | LPAREN; e=expr; RPAREN { e }
;

cmd:
  | SKIP; { Skip }
  | BREAK; { Break }
  | x=ID; ASSIGN; e=expr; { Assign(x,e) }
  | x=ID; LBRACKET; e1=expr; RBRACKET; ASSIGN; e2=expr; { Assign_arr(x,e1,e2) }
  | c1=cmd; SEQ; c2=cmd; { Seq(c1,c2) }
  | REPEAT; c=cmd; FOREVER; { Repeat c }
  | IF; e1=expr; THEN; c1=cmd; ELSE; c2=cmd; { If(e1,c1,c2) }
  | LBRACE; d=decl_v; SEQ; c=cmd; RBRACE; { Block(d,c) }
  | x=ID; LPAREN; p=parama; RPAREN; { Call(x,p) }
  | LBRACE; c=cmd; RBRACE; { c }
  | LPAREN; c=cmd; RPAREN; { c }
;

decl_v:
  | d1=decl_v; SEQ; d2=decl_v; { DvSeq(d1,d2) }
  | INTDEC; x=ID; { IntVar x }
  | ARRAY; x=ID; LBRACKET; n=CONST; RBRACKET; { IntArr(x,n) }
  | { NullVar }
;

decl_p:
  | PROC; x=ID; LPAREN; p=paramf; RPAREN; LBRACE; c=cmd; RBRACE; { Proc(x,p,c) }

declplist:
  | SEQ; dl=nonempty_list(decl_p); SEQ; { DeclList dl }
  | { DeclList [] }
;
  
paramf:
  | VAL; x=ID; { Val x }
  | REF; x=ID; { Ref x }
;

parama: | e=expr; { e };
