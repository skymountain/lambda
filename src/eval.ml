open Misc
open Syntax

exception Eval_error of string
  
type value =
    IntV of int
  | FunV of id * exp * (id * value) Env.t
  | TypeFunV of exp * (id * value) Env.t
let err s = raise (Eval_error (Printf.sprintf "Runtime error: %s" s))
      
let pps_val = function
    IntV i -> string_of_int i
  | FunV _ -> "<fun>"
  | TypeFunV _ -> "<type fun>"

let pp_val v =
  print_string @< pps_val v

let eval_binop = function
    (Plus, IntV vl, IntV vr) -> IntV (vl + vr)
  | (Mult, IntV vl, IntV vr) -> IntV (vl * vr)
  | (Div, IntV vl, IntV vr) when vr <> 0  -> IntV (vl / vr)
  | (Div, IntV _, IntV _)  -> err "division by zero isn't allowed"
  | (Plus, _, _) -> err "both arguments of + must be integer"
  | (Mult, _, _) -> err "both arguments of * must be integer"
  | (Div, _, _)  -> err "both arguments of / must be integer"
      
let rec eval_exp env = function
    Var id ->
      begin match Env.lookup env id with
        None -> err @< Printf.sprintf "%s is not bound" id
      | Some v -> v
      end
  | IntLit i -> IntV i
  | BinOp (op, el, er) -> eval_binop (op, eval_exp env el, eval_exp env er)
  | Fun (para, body) -> FunV (para, body, env)
  | App (f, act) ->
      begin match eval_exp env f, eval_exp env act with
        FunV (formal, body, env'), act ->
          eval_exp (Env.extend env' formal act) body
      | _ -> err "not-function value is applied"
      end
  | Let (var, e, body) ->
      let v = eval_exp env e in
      eval_exp (Env.extend env var v) body
        
let eval env = function
    Exp exp -> Some (env, "it", eval_exp env exp)
  | Decl (var, exp) -> Some (env, var, eval_exp env exp)
  | EOF -> None
