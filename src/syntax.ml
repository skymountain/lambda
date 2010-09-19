type id = string

type binop =
    Plus
  | Mult
  | Div
      
type typ =
    IntT
  | FunT of typ * typ
      
type exp =
    Var     of id
  | IntLit  of int
  | BinOp   of binop * exp * exp
  | Fun     of id * typ * exp
  | App     of exp * exp
  | Let     of id * exp * exp
      
type program =
    Exp  of exp
  | Decl of id * exp
  | EOF
