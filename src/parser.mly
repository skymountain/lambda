%{
  open Syntax

  let rec mk_fun bounds exp = match bounds with
    | [] -> exp
    | (var, typ)::bounds -> Fun (var, typ, mk_fun bounds exp)

  let mk_let ident bounds typ exp body =
    let t = Let (ident, mk_fun bounds exp, body) in
    match typ with
    | Some typ -> TypedExpr (t, typ)
    | None     -> t

  let mk_letdecl ident bounds typ exp =
    let t = mk_fun bounds exp in
    let t = match typ with
    | Some typ -> TypedExpr (t, typ)
    | None     -> t
    in
    Decl (ident, t)

  let mk_reclet ident bounds typ exp body =
    LetRec (ident, typ, mk_fun bounds exp, body)

  let mk_recletdecl ident bounds typ exp =
    DeclRec (ident, typ, mk_fun bounds exp)
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
  Expr                                         { Exp $1 }
| LET Ident BoundVarList WithType EQ Expr    { mk_letdecl $2 $3 $4 $6 }
| LET REC Ident BoundVarList COLON TypeExpr EQ Expr
                                               { mk_recletdecl $3 $4 $6 $8 }

Expr:
| AppExpr { $1 }
      
| BACKSLA BoundVarListMore DOT Expr          { mk_fun $2 $4 }
| LET Ident BoundVarList WithType EQ Expr IN Expr
                                               { mk_let $2 $3 $4 $6 $8 }
| LET REC Ident BoundVarList COLON TypeExpr EQ Expr IN Expr
                                               { mk_reclet $3 $4 $6 $8 $10 }
| IF Expr THEN Expr ELSE Expr                  { IfExp ($2, $4, $6) }
| MATCH Expr WITH MatchExpr                    { MatchExp ($2, $4) }

| Expr INFIXOP0 Expr { App (App (Var $2, $1), $3) }
| Expr INFIXOP1 Expr { App (App (Var $2, $1), $3) }
| Expr INFIXOP2 Expr { App (App (Var $2, $1), $3) }
| Expr INFIXOP3 Expr { App (App (Var $2, $1), $3) }
| Expr INFIXOP4 Expr { App (App (Var $2, $1), $3) }
| Expr EQ       Expr { App (App (Var "=", $1), $3) }
| Expr COLON2 Expr   { BinOp (BCons,  $1, $3) }

SExpr:
  PREFIXOP SExpr     { App (Var $1, $2) }
| ConstExpr          { Const $1 }
| ListExpr           { $1 }
| Ident              { Var $1 }
| LPAREN Expr RPAREN { $2 }
| LPAREN Expr COLON TypeExpr RPAREN
                     { TypedExpr ($2, $4) }
| LPAREN UIDENT COLON TypeExpr RPAREN
                     { Construct ($2, $4) }

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
  UNDERBAR                       { PWildCard }
| Ident                          { PVar $1 }
| ConstExpr                      { PConst $1 }
| PatternExpr AS Ident           { PAs ($1, $3) }
| LSQPAREN PListExpr RSQPAREN    { PList $2 }
| PatternExpr COLON2 PatternExpr { PCons ($1, $3) }
| PatternExpr VBAR PatternExpr   { POr ($1, $3) }
| LPAREN UIDENT COLON TypeExpr RPAREN
                                 { PConstr ($2, $4, []) }
| LPAREN UIDENT COLON TypeExpr RPAREN LPAREN PatternExprList RPAREN
                                 { PConstr ($2, $4, $7) }
| LPAREN PatternExpr RPAREN      { $2 }

PListExpr:
  PListExpr_ { [$1] }
| PListExpr_ SEMICOLON PListExpr { $1::$3 }
PListExpr_:
  PatternExpr { $1 }

PatternExprList:
  PatternExpr                       { [$1] }
| PatternExpr COMMA PatternExprList { $1::$3 }

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

BoundVarList:
|                      { [] }
| BoundVarListMore   { $1 }

BoundVarListMore:
| Ident COLON TypeExpr { [$1, $3] }
| BoundVarParenList  { $1 }

BoundVarParenList:
| LPAREN Ident COLON TypeExpr RPAREN { [$2, $4] }
| LPAREN Ident COLON TypeExpr RPAREN BoundVarParenList
                                     { ($2, $4)::$6 }

WithType:
|                { None }
| COLON TypeExpr { Some $2 }

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
  TypeExpr RARROW TypeExpr            { TFun ($1, $3) }
| TypeExpr_                           { $1 }
TypeExpr_:
  LIDENT                              { TName ([], $1) }
| TypeExpr_ LIDENT                    { TName ([$1], $2) }
| LPAREN MultiTypeExprs RPAREN LIDENT { TName ($2, $4) }
| LPAREN TypeExpr RPAREN              { $2 }
| TypeParameter                       { TVar $1 }

MultiTypeExprs:
| TypeExpr COMMA TypeExpr             { [$1; $3] }
| TypeExpr COMMA MultiTypeExprs       { $1::$3 }

TypeExprList:
| TypeExpr                            { [$1] }
| MultiTypeExprs                      { $1 }
