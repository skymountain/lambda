%{
  open Syntax
%}

%token BACKSLA DOT SEMICOLON2
%token LPAREN RPAREN  
%token PLUS ASTER SLASH MINUS LT
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
  
%token<Syntax.id> IDENT
%token<int> INTLIT
%token EOF

%nonassoc AS /* below binary operator (COLON2) */
%nonassoc below_VBAR
%left VBAR
%left EQ LT
%right COLON2
%left PLUS MINUS
%left ASTER SLASH
%right RARROW
%nonassoc LIST
  
%start main
%type<Syntax.program> main
%%
  
main:
  Expr SEMICOLON2 { Exp $1 }
| LET IDENT EQ Expr SEMICOLON2 { Decl ($2, $4) }
| LET REC IDENT COLON TypeExpr EQ Expr SEMICOLON2 { DeclRec ($3, $5, $7) }
| EOF { Syntax.EOF }

Expr:
  BACKSLA IDENT COLON TypeExpr DOT Expr { Fun ($2, $4, $6) }
| LET IDENT EQ Expr IN Expr { Let ($2, $4, $6) }
| LET REC IDENT COLON TypeExpr EQ Expr IN Expr { LetRec ($3, $5, $7, $9) }
| IF Expr THEN Expr ELSE Expr { IfExp ($2, $4, $6) }
| MATCH Expr WITH MatchExpr   { MatchExp ($2, $4) }
| SExpr { $1 }

SExpr:
  BinExpr { $1 }
      
BinExpr:
  BinExpr PLUS   BinExpr { BinOp (Plus,  $1, $3) }
| BinExpr MINUS  BinExpr { BinOp (Minus, $1, $3) }
| BinExpr ASTER  BinExpr { BinOp (Mult,  $1, $3) }
| BinExpr SLASH  BinExpr { BinOp (Div,   $1, $3) }
| BinExpr LT     BinExpr { BinOp (Lt,    $1, $3) }
| BinExpr COLON2 BinExpr { BinOp (Cons,  $1, $3) }
| AppExpr                { $1 }

AppExpr:
  AppExpr PrefixExpr { App ($1, $2) }
| PrefixExpr { $1 }

PrefixExpr:
  AtomExpr { $1 }

AtomExpr:
  ConstExpr          { Const $1 }
| ListExpr           { $1 }
| IDENT              { Var $1 }
| LPAREN Expr RPAREN { $2 }
| LPAREN Expr COLON TypeExpr RPAREN { TypedExpr ($2, $4) }
      
ConstExpr:
  INTLIT            { CInt $1 }
| TRUE              { CBool true }
| FALSE             { CBool false }
| LPAREN LSQPAREN RSQPAREN COLON TypeExpr RPAREN
                    { CNullList $5 }

ListExpr:
  LSQPAREN SeqExpr RSQPAREN { ListLit $2 }

SeqExpr:
  Expr SEMICOLON SeqExpr { $1::$3 }
| Expr  { [$1] }
  
MatchExpr:
  MatchExpr_ VBAR MatchExpr   { $1::$3 }
| MatchExpr_ %prec below_VBAR { [$1] }

MatchExpr_:
  PatternExpr RARROW Expr  { ($1, $3) }
      
PatternExpr:
  UNDERBAR                       { WildCard }
| IDENT                          { PVar $1 }
| ConstExpr                      { PConst $1 }
| PatternExpr AS IDENT           { As ($1, $3) }
| LSQPAREN PListExpr RSQPAREN    { PList $2 }
| PatternExpr COLON2 PatternExpr { PCons ($1, $3) }
| PatternExpr VBAR PatternExpr   { POr ($1, $3) }
| LPAREN PatternExpr RPAREN      { $2 }
      
PListExpr:
  PListExpr_ { [$1] }
| PListExpr_ SEMICOLON PListExpr { $1::$3 }
      
PListExpr_:
  PatternExpr { $1 }
      
TypeExpr:
  INT                      { IntT }
| BOOL                     { BoolT }
| TypeExpr LIST            { ListT $1 }
| TypeExpr RARROW TypeExpr { FunT ($1, $3) }
| LPAREN TypeExpr RPAREN   { $2 }
