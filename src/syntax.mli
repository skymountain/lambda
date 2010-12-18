type id = string;;

type binop =
    BPlus
  | BMinus
  | BMult
  | BDiv
  | BLt
  | BCons

val str_of_binop : binop -> string

type typ =
  | TFun  of typ * typ
  | TVar  of id
  | TName of typ list * id

type const =
    CInt      of int
  | CBool     of bool
  | CNullList of typ

(* pattern *)
type pat =
    PVar   of id
  | PWildCard
  | PConst of const
  | PAs    of pat * id
  | PList  of pat list
  | PCons  of pat * pat
  | POr    of pat * pat
  | PConstr of string * typ * pat list

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
  | Construct of string * typ

type eval =
    Exp  of exp
  | Decl of id * exp
  | DeclRec of id * typ * exp

type typ_kind =
    TkAlias of typ
  | TkVariant of (string * typ list) list

type typdef = { td_name: string; td_params: string list; td_kind: typ_kind; }

type program =
  | Eval of eval
  | TypDef of typdef
  | EOF
