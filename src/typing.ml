open Misc
open Syntax

exception Typing_error of string
let err s = raise (Typing_error (Printf.sprintf "Typing error: %s" s))

(* pretty printer for type *)
let rec pps_typ =
  let is_funtyp_without_paren s =
    let len = String.length s in
    let rec iter idx depth =
      if idx >= len then false
      else match s.[idx] with
        '(' -> iter (idx+1) (depth+1)
      | ')' -> iter (idx+1) (depth-1)
      | '-' when idx+1 < len && s.[idx+1] = '>'-> true
      | _   -> iter (idx+1) depth
    in
    iter 0 0
  in
  function
    IntT  -> "int"
  | BoolT -> "bool"
  | FunT (typ1, typ2) -> begin
      let t = pps_typ typ1 in
      let t = if is_funtyp_without_paren t then "("^t^")" else t in
      t ^ " -> " ^ (pps_typ typ2)
    end

let rec pp_typ typ =
  print_string @< pps_typ typ

(* equality function *)
let rec eq_typ typ1 typ2 =
  match typ1, typ2 with
    IntT, IntT -> true
  | FunT (arg1, ret1), FunT (arg2, ret2) ->
      eq_typ arg1 arg2 && eq_typ ret1 ret2
  | BoolT, BoolT -> true
  | _ -> false

(* typing for binary operator *)
let str_of_binop = function
    Plus  -> "+"
  | Mult  -> "*"
  | Div   -> "/"
  | Equal -> "="
      
let typ_binop typ1 typ2 = function
    (Plus | Mult | Div) as op ->
      if typ1 = IntT && typ2 = IntT then IntT
      else err @< Printf.sprintf "both arguments of %s must be integer" @< str_of_binop op
  | Equal ->
      if eq_typ typ1 typ2 then BoolT
      else err @< Printf.sprintf "both arguments of %s must be same types" @< str_of_binop Equal
        
(* typing for exp *)
let rec typ_exp tenv = function
    Var var -> begin
      match Env.lookup tenv var with
        Some t -> t
      | None   -> err @< Printf.sprintf "%s is not bound" var
    end
  | IntLit _  -> IntT
  | BoolLit _ -> BoolT
  | BinOp (op, e1, e2)  ->
      let typ1, typ2 = typ_exp tenv e1, typ_exp tenv e2 in
      typ_binop typ1 typ2 op
  | IfExp (cond, then_exp, else_exp) -> begin
      match typ_exp tenv cond with
        BoolT -> begin
          let then_typ = typ_exp tenv then_exp in
          let else_typ = typ_exp tenv else_exp in
          if eq_typ then_typ else_typ then then_typ
          else err "types of then and else expressions must be same"
        end
      | _ -> err "type of conditional expression must be boolean"
    end
  | Fun (var, typ, body) ->
      FunT (typ, typ_exp (Env.extend tenv var typ) body)
  | App (e1, e2) -> begin
      match typ_exp tenv e1 with
        FunT (arg_typ, ret_typ) ->
          if eq_typ arg_typ @< typ_exp tenv e2 then ret_typ
          else err "type of actual argument must correspond with one of formal argument"
      | _ -> err "only function type can be applied"
    end
  | Let (var, e, body) ->
      typ_exp (Env.extend tenv var @< typ_exp tenv e) body

(* typing for program *)
let typing tenv =
  let return tenv var typ =
    Env.extend tenv var typ, var, typ
  in
  function
    Exp e -> return tenv "it" @< typ_exp tenv e
  | Decl (var, e) -> return tenv var @< typ_exp tenv e
  | EOF -> assert false
