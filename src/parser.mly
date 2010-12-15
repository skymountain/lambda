%{
  open Syntax
%}

%token BACKSLA DOT SEMICOLON2
%token LPAREN RPAREN  
%token RARROW COLON
%token EQ
%token LET IN
%token TRUE FALSE
%token IF THEN ELSE
%token REC
%token LSQPAREN RSQPAREN SEMICOLON COLON2
%token BEGIN END
%token MATCH WITH VBAR
%token UNDERBAR
%token AS
%token<Syntax.id> PREFIXOP INFIXOP0 INFIXOP1 INFIXOP2 INFIXOP3 INFIXOP4
%token<Syntax.id> LIDENT
%token<Syntax.id> UIDENT
%token<int> INTLIT
%token COMMA
%token OF
%token QUOTE
%token TYPE
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
  Eval SEMICOLON2    { Eval $1 }
| TypeDef SEMICOLON2 { TypDef $1 }
| EOF                { Syntax.EOF }

Eval:
  Expr                                 { Exp $1 }
| LET Ident EQ Expr                    { Decl ($2, $4) }
| LET REC Ident COLON TypeExpr EQ Expr { DeclRec ($3, $5, $7) }

Expr:
  SExpr { $1 }
| AppExpr SExpr { App ($1, $2) }
      
| BACKSLA Ident COLON TypeExpr DOT Expr        { Fun ($2, $4, $6) }
| LET Ident EQ Expr IN Expr                    { Let ($2, $4, $6) }
| LET REC Ident COLON TypeExpr EQ Expr IN Expr { LetRec ($3, $5, $7, $9) }
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
| UIDENT             { Construct $1 }

AppExpr:
  AppExpr SExpr { App ($1, $2) }
| SExpr { $1 }

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
| Ident                          { PVar $1 }
| ConstExpr                      { PConst $1 }
| PatternExpr AS Ident           { As ($1, $3) }
| LSQPAREN PListExpr RSQPAREN    { PList $2 }
| PatternExpr COLON2 PatternExpr { PCons ($1, $3) }
| PatternExpr VBAR PatternExpr   { POr ($1, $3) }
| LPAREN PatternExpr RPAREN      { $2 }
      
PListExpr:
  PListExpr_ { [$1] }
| PListExpr_ SEMICOLON PListExpr { $1::$3 }
      
PListExpr_:
  PatternExpr { $1 }
      

Ident:
  LIDENT                 { $1 }
| LPAREN Operator RPAREN { $2 }

Operator:
  PREFIXOP { $1 }
| INFIXOP0 { $1 }
| INFIXOP1 { $1 }
| INFIXOP2 { $1 }
| INFIXOP3 { $1 }
| INFIXOP4 { $1 }
| EQ       { "=" }

TypeDef:
  TYPE TypeParams LIDENT EQ TypeDefinition { { td_name = $3; td_params = $2; td_kind = $5 } }

TypeParams:
                            { [] }
| TypeParameter             { [$1] }
| LPAREN TypeParams_ RPAREN { $2 }
TypeParams_:
  TypeParameter                   { [$1] }
| TypeParameter COMMA TypeParams_ { $1::$3 }

TypeParameter:
  QUOTE LIDENT              { $2 }
| QUOTE UIDENT              { $2 }

TypeDefinition:
  TypeExpr                  { TkAlias $1 }
| VariantDefinitionList     { TkVariant $1 }

VariantDefinitionList:
  VariantDefinition                            { [$1] }
| VariantDefinition VBAR VariantDefinitionList { $1::$3 }

VariantDefinition:
  UIDENT                 { ($1, []) }
| UIDENT OF TypeExprList { ($1, $3) }


TypeExpr:
  TypeExpr RARROW TypeExpr          { FunT ($1, $3) }
| TypeExpr_                         { $1 }
TypeExpr_:
  LIDENT                            { NameT ([], $1) }
| TypeExpr_ LIDENT                  { NameT ([$1], $2) }
| LPAREN TypeExprList RPAREN LIDENT { NameT ($2, $4) }
| TypeParameter                     { VarT $1 }
| LPAREN TypeExpr RPAREN            { $2 }

TypeExprList:
  TypeExpr                    { [$1] }
| TypeExpr COMMA TypeExprList { $1::$3 }
