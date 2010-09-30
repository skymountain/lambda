%{
  open Syntax
%}

%token BACKSLA DOT SEMICOLON2
%token LPAREN RPAREN  
%token RARROW COLON
%token EQ
%token LET IN
%token INT
%token BOOL TRUE FALSE
%token IF THEN ELSE
%token REC
%token LIST LSQPAREN RSQPAREN SEMICOLON COLON2
%token BEGIN END
%token MATCH WITH VBAR
%token UNDERBAR
%token AS
%token<Syntax.id> PREFIXOP INFIXOP0 INFIXOP1 INFIXOP2 INFIXOP3 INFIXOP4
  
%token<Syntax.id> IDENT
%token<int> INTLIT
%token EOF

%nonassoc IN
%nonassoc LET
%nonassoc BACKSLA WITH
%nonassoc THEN
%nonassoc ELSE
%nonassoc AS /* below binary operator (COLON2) */
%nonassoc below_VBAR
%left VBAR
%right RARROW DOT
%left INFIXOP0 EQ
%right INFIXOP1
%right COLON2
%left INFIXOP2
%left INFIXOP3
%right INFIXOP4
%nonassoc LIST PREFIXOP
  
%start main
%type<Syntax.program> main
%%
  
main:
  Expr SEMICOLON2 { Exp $1 }
| LET Ident EQ Expr SEMICOLON2                    { Decl ($2, $4) }
| LET REC Ident COLON TypeExpr EQ Expr SEMICOLON2 { DeclRec ($3, $5, $7) }
| LET REC Ident EQ Expr SEMICOLON2                { DeclRec ($3, Type.fresh_typvar (), $5) }
| EOF { Syntax.EOF }

Expr:
  SExpr { $1 }
| AppExpr SExpr { App ($1, $2) }
      
| BACKSLA Ident COLON TypeExpr DOT Expr        { Fun ($2, $4, $6) }
| BACKSLA Ident DOT Expr                       { Fun ($2, Type.fresh_typvar (), $4) }
| LET Ident EQ Expr IN Expr                    { Let ($2, $4, $6) }
| LET REC Ident COLON TypeExpr EQ Expr IN Expr { LetRec ($3, $5, $7, $9) }
| LET REC Ident EQ Expr IN Expr                { LetRec ($3, Type.fresh_typvar (), $5, $7) }
| IF Expr THEN Expr ELSE Expr                  { IfExp ($2, $4, $6) }
| MATCH Expr WITH MatchExpr                    { MatchExp ($2, $4) }
      
| Expr INFIXOP0 Expr { App (App (Var $2, $1), $3) }
| Expr INFIXOP1 Expr { App (App (Var $2, $1), $3) }
| Expr INFIXOP2 Expr { App (App (Var $2, $1), $3) }
| Expr INFIXOP3 Expr { App (App (Var $2, $1), $3) }
| Expr INFIXOP4 Expr { App (App (Var $2, $1), $3) }
| Expr EQ       Expr { App (App (Var "=", $1), $3) }
| Expr COLON2 Expr   { BinOp (Cons,  $1, $3) }
      
SExpr:
  PREFIXOP SExpr     { App (Var $1, $2) }
| ConstExpr          { Const $1 }
| ListExpr           { $1 }
| Ident              { Var $1 }
| LPAREN Expr RPAREN { $2 }
| LPAREN Expr COLON TypeExpr RPAREN
                     { TypedExpr ($2, $4) }

AppExpr:
  AppExpr SExpr { App ($1, $2) }
| SExpr { $1 }
      
ConstExpr:
  INTLIT            { CInt $1 }
| TRUE              { CBool true }
| FALSE             { CBool false }
| LSQPAREN RSQPAREN { CNullList }

ListExpr:
  LSQPAREN SeqExpr RSQPAREN { $2 }

SeqExpr:
  Expr SEMICOLON SeqExpr { BinOp (Cons, $1, $3) }
| Expr  { BinOp (Cons, $1, Const CNullList) }
  
MatchExpr:
  MatchExpr_ VBAR MatchExpr   { $1::$3 }
| MatchExpr_ %prec below_VBAR { [$1] }

MatchExpr_:
  PatternExpr RARROW Expr  { ($1, $3) }
      
PatternExpr:
  UNDERBAR                       { WildCard }
| Ident                          { PVar $1 }
| ConstExpr                      { PConst $1 }
| PatternExpr AS Ident           { As ($1, $3) }
| LSQPAREN PListExpr RSQPAREN    { $2 }
| PatternExpr COLON2 PatternExpr { PCons ($1, $3) }
| PatternExpr VBAR PatternExpr   { POr ($1, $3) }
| LPAREN PatternExpr RPAREN      { $2 }
      
PListExpr:
  PListExpr_ { PCons ($1, PConst CNullList) }
| PListExpr_ SEMICOLON PListExpr { PCons ($1, $3) }
      
PListExpr_:
  PatternExpr { $1 }
      
TypeExpr:
  INT                      { IntT }
| BOOL                     { BoolT }
| TypeExpr LIST            { ListT $1 }
| TypeExpr RARROW TypeExpr { FunT ($1, $3) }
| LPAREN TypeExpr RPAREN   { $2 }

Ident:
  IDENT                  { $1 }
| LPAREN operator RPAREN { $2 }

operator:
  PREFIXOP { $1 }
| INFIXOP0 { $1 }
| INFIXOP1 { $1 }
| INFIXOP2 { $1 }
| INFIXOP3 { $1 }
| INFIXOP4 { $1 }
| EQ       { "=" }
