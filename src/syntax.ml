type id = string

type binop =
    Plus
  | Minus
  | Mult
  | Div
  | Lt
  | Cons
      
let str_of_binop = function
    Plus  -> "+"
  | Minus -> "-"
  | Mult  -> "*"
  | Div   -> "/"
  | Lt    -> "<"
  | Cons  -> "::"
      
type typ =
    IntT
  | BoolT
  | FunT  of typ * typ
  | ListT of typ
  | VarT  of id
   
type const =
    CInt      of int
  | CBool     of bool
  | CNullList of typ
      
(* pattern *)      
type pat =
    PVar   of id
  | WildCard
  | PConst of const
  | As     of pat * id
  | PList  of pat list
  | PCons  of pat * pat
  | POr    of pat * pat
      
type exp =
    Var     of id
  | Const   of const
  | BinOp   of binop * exp * exp
  | Fun     of id * typ * exp
  | App     of exp * exp
  | Let     of id * exp * exp
  | IfExp   of exp * exp * exp
  | LetRec  of id * typ * exp * exp
  | ListLit of exp list
  | TypedExpr of exp * typ      
  | MatchExp of exp * (pat * exp) list

type eval =
    Exp  of exp
  | Decl of id * exp
  | DeclRec of id * typ * exp

type typ_kind =
    TdExp of typ
  | TdVariance of (string * typ list) list

type typdef = { td_name: string; td_params: string list; td_kind: typ_kind; }

type program =
  | Eval of eval
  | TypDef of typdef
  | EOF
