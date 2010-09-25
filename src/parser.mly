%{
  open Syntax
%}

%token BACKSLA DOT SEMICOLON2
%token LPAREN RPAREN  
%token PLUS ASTER SLASH MINUS
%token RARROW COLON
%token EQ
%token LET IN
%token INT
%token BOOL TRUE FALSE
%token IF THEN ELSE
%token REC
  
%token<Syntax.id> IDENT
%token<int> INTLIT
%token EOF

%left EQ
%left PLUS MINUS
%left ASTER SLASH
%right RARROW
  
%start main
%type<Syntax.program> main
%%
  
main:
  Expr SEMICOLON2 { Exp $1 }
| LET IDENT EQ Expr SEMICOLON2 { Decl ($2, $4) }
| LET REC IDENT COLON TypeExpr EQ Expr SEMICOLON2 { DeclRec ($3, $5, $7) }
| LET REC IDENT EQ Expr SEMICOLON2 { DeclRec ($3, Type.fresh_typvar (), $5) }
| EOF { Syntax.EOF }

Expr:
  BACKSLA IDENT COLON TypeExpr DOT Expr { Fun ($2, $4, $6) }
| BACKSLA IDENT DOT Expr { Fun ($2, Type.fresh_typvar (), $4) }
| LET IDENT EQ Expr IN Expr { Let ($2, $4, $6) }
| LET REC IDENT COLON TypeExpr EQ Expr IN Expr { LetRec ($3, $5, $7, $9) }
| LET REC IDENT EQ Expr IN Expr { LetRec ($3, Type.fresh_typvar (), $5, $7) }
| IF Expr THEN Expr ELSE Expr { IfExp ($2, $4, $6) }
| ArithExpr { $1 }

ArithExpr:
  ArithExpr PLUS  ArithExpr { BinOp (Plus,  $1, $3) }
| ArithExpr MINUS ArithExpr { BinOp (Minus, $1, $3) }
| ArithExpr ASTER ArithExpr { BinOp (Mult,  $1, $3) }
| ArithExpr SLASH ArithExpr { BinOp (Div,   $1, $3) }
| ArithExpr EQ    ArithExpr { BinOp (Eq,    $1, $3) }
| AppExpr         { $1 }

AppExpr:
  AppExpr AtomExpr { App ($1, $2) }
| AtomExpr { $1 }

AtomExpr:
  INTLIT  { IntLit $1 }
| TRUE    { BoolLit true }
| FALSE   { BoolLit false }
| IDENT   { Var $1 }
| LPAREN Expr RPAREN { $2 }

TypeExpr:
  INT        { IntT }
| BOOL       { BoolT }
| TypeExpr RARROW TypeExpr { FunT ($1, $3) }
| LPAREN TypeExpr RPAREN { $2 }
