%{
  open Syntax
%}

%token BACKSLA DOT SEMICOLON2
%token LPAREN RPAREN  
%token PLUS ASTER SLASH
%token RARROW COLON
%token EQ
%token LET IN
%token INT
  
%token<Syntax.id> IDENT
%token<int> INTLIT
%token EOF
  
%left PLUS
%left ASTER SLASH
%right RARROW
  
%start main
%type<Syntax.program> main
%%
  
main:
  Expr SEMICOLON2 { Exp $1 }
| LET IDENT EQ Expr SEMICOLON2 { Decl ($2, $4) }
| EOF { Syntax.EOF }

Expr:
  BACKSLA IDENT COLON TypeExpr DOT Expr { Fun ($2, $4, $6) }
| LET IDENT EQ Expr IN Expr { Let ($2, $4, $6) }
| ArithExpr { $1 }

ArithExpr:
  ArithExpr PLUS  ArithExpr { BinOp (Plus, $1, $3) }
| ArithExpr ASTER ArithExpr { BinOp (Mult, $1, $3) }
| ArithExpr SLASH ArithExpr { BinOp (Div,  $1, $3) }
| AppExpr         { $1 }

AppExpr:
  AppExpr AtomExpr { App ($1, $2) }
| AtomExpr { $1 }

AtomExpr:
  INTLIT { IntLit $1 }
| IDENT  { Var $1 }
| LPAREN Expr RPAREN { $2 }

TypeExpr:
  INT        { IntT }
| TypeExpr RARROW TypeExpr { FunT ($1, $3) }
| LPAREN TypeExpr RPAREN { $2 }
