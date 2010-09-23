type id = string

type binop =
    Plus
  | Minus
  | Mult
  | Div
  | Eq
let str_of_binop = function
    Plus  -> "+"
  | Minus -> "-"
  | Mult  -> "*"
  | Div   -> "/"
  | Eq -> "="
      
type typ =
    IntT
  | BoolT
  | FunT of typ * typ
      
type exp =
    Var     of id
  | IntLit  of int
  | BoolLit of bool
  | BinOp   of binop * exp * exp
  | Fun     of id * typ * exp
  | App     of exp * exp
  | Let     of id * exp * exp
  | IfExp   of exp * exp * exp
  | LetRec  of id * typ * exp * exp
      
type program =
    Exp  of exp
  | Decl of id * exp
  | DeclRec of id * typ * exp
  | EOF
