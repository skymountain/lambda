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
      
type program =
    Exp  of exp
  | Decl of id * exp
  | DeclRec of id * typ * exp
  | EOF
