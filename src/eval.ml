open Misc
open Syntax

exception Eval_error of string
  
type value =
    IntV of int
  | FunV of id * exp * (id * value) Env.t
  | TypeFunV of exp * (id * value) Env.t
let err s = raise (Eval_error (Printf.sprintf "Runtime error: %s" s))

(* pretty printer for value *)
let pps_val = function
    IntV i -> string_of_int i
  | FunV _ -> "<fun>"
  | TypeFunV _ -> "<type fun>"

let pp_val v =
  print_string @< pps_val v

(* evaluation for binary operator *)
let eval_binop = function
    (Plus, IntV vl, IntV vr) -> IntV (vl + vr)
  | (Mult, IntV vl, IntV vr) -> IntV (vl * vr)
  | (Div, IntV vl, IntV vr) when vr <> 0  -> IntV (vl / vr)
  | (Div, IntV _, IntV _)  -> err "division by zero isn't allowed"
  | (Plus, _, _) -> err "both arguments of + must be integer"
  | (Mult, _, _) -> err "both arguments of * must be integer"
  | (Div, _, _)  -> err "both arguments of / must be integer"

(* evaluation for exp *)
let rec eval_exp env = function
    Var var ->
      begin match Env.lookup env var with
        None -> err @< Printf.sprintf "%s is not bound" var
      | Some v -> v
      end
  | IntLit i -> IntV i
  | BinOp (op, el, er) -> eval_binop (op, eval_exp env el, eval_exp env er)
  | Fun (var, _, body) -> FunV (var, body, env)
  | App (f, act) ->
      begin match eval_exp env f, eval_exp env act with
        FunV (formal, body, env'), act ->
          eval_exp (Env.extend env' formal act) body
      | _ -> err "not-function value is applied"
      end
  | Let (var, e, body) ->
      let v = eval_exp env e in
      eval_exp (Env.extend env var v) body

(* evaluation for program *)
let eval env =
  let return env var v =
    Env.extend env var v, var, v
  in
  function
    Exp exp -> return env "it" @< eval_exp env exp
  | Decl (var, exp) -> return env var @< eval_exp env exp
  | EOF -> assert false
