%{
open Ast
%}

%token INT
%token ARRAY
%token PLUS
%token MINUS
%token TIMES
%token TRUE
%token FALSE
%token AND
%token OR
%token NOT
%token EQ
%token LEQ
%token <string> ID
%token <string> CONST

%token SKIP
%token BREAK
%token TAKES
%token SEQ
%token REPEAT
%token FOREVER
%token IF
%token THEN
%token ELSE

%token LPAREN
%token RPAREN
%token LSQPAREN
%token RSQPAREN
%token LBRACKET
%token RBRACKET

%token PROC
%token RETURN
%token REF
%token VAL

%token EOF

%left SEQ
%nonassoc ELSE 
%left OR
%left AND
%nonassoc NOT
%left EQ LEQ
%left PLUS MINUS
%left TIMES

%start <prog> prog

%%

prog:
  | d = decl; c = cmd; EOF { Prog(d,c) }
  ;

/* TODO: dichiarazione vettore */
expr:
  | i = CONST { Const(int_of_string i) }
  | TRUE { True }
  | FALSE { False }
  | e1 = expr; PLUS; e2=expr { Plus(e1,e2) }
  | e1 = expr; MINUS; e2=expr { Minus(e1,e2) }
  | e1 = expr; TIMES; e2=expr { Mul(e1,e2) }
  | e1 = expr; AND; e2=expr { And(e1,e2) }
  | e1 = expr; OR; e2=expr { Or(e1,e2) }
  | NOT; e=expr { Not e }
  | e1 = expr; EQ; e2=expr { Eq(e1,e2) }
  | e1 = expr; LEQ; e2=expr { Leq(e1,e2) }
  | x = ID { Var(x) }
  | LPAREN; e = expr; RPAREN { e }

/*  Assegnamento vettore 
    Repeat
    Call*/
cmd:
  | SKIP { Skip }
  | BREAK { Break }
  | c1=cmd; SEQ; c2=cmd { Seq(c1,c2) }
  | x = ID; TAKES; e=expr; { Assign(x,e) }
  | x = ID; LSQPAREN; i=CONST; RSQPAREN; TAKES; e=expr { Assign_arr(x,i,e) }
  | IF; e=expr; THEN; c1=cmd; ELSE; c2=cmd {If(e,c1,c2)}
  | LBRACKET; dv=decl_v; c=cmd; RBRACKET { Block(dv,c) }

/* Dichiarazioni di procedure */
decl:
  | INT; x = ID { IntVar(x) }
  | ARRAY; x = ID; LSQPAREN; i=CONST; RSQPAREN { IntArr(x,i) }


