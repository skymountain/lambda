type id = string

type unaryop =
    Deref

let str_of_unaryop = function
    Deref  -> "!"
      
type binop =
    Plus
  | Minus
  | Mult
  | Div
  | Lt
  | Cons
  | Assign
      
let str_of_binop = function
    Plus   -> "+"
  | Minus  -> "-"
  | Mult   -> "*"
  | Div    -> "/"
  | Lt     -> "<"
  | Cons   -> "::"      
  | Assign -> ":="

type typvar = int
      
type typ =
    IntT
  | BoolT
  | TypVar of typvar
  | FunT   of typ * typ
  | ListT  of typ
  | RefT   of typ
      
type const =
    CInt      of int
  | CBool     of bool
  | CNullList
      
(* pattern *)      
type pat =
    PVar   of id
  | WildCard
  | PConst of const
  | As     of pat * id
  | PCons  of pat * pat
  | POr    of pat * pat
      
type exp =
    Var     of id
  | Const   of const
  | UnaryOp of unaryop * exp
  | BinOp   of binop * exp * exp
  | Fun     of id * typ * exp
  | App     of exp * exp
  | Let     of id * exp * exp
  | IfExp   of exp * exp * exp
  | LetRec  of id * typ * exp * exp
  | TypedExpr of exp * typ      
  | MatchExp of exp * (pat * exp) list
  | RefExp  of exp
      
type program =
    Exp  of exp
  | Decl of id * exp
  | DeclRec of id * typ * exp
  | EOF
