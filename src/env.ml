open Misc
open Common

type 'a t = 'a list

let empty = []
  
let extend env key v = (key, v)::env
let extendl env l = l @ env
let lookup env key =
  try Some (List.assoc key env) with
    _ -> None

let mem env key =
  match lookup env key with
    None -> false
  | _    -> true

let list_of env = env

let rec pps_env = function
    [] -> "[]"
  | (key,_)::xs -> Printf.sprintf "key:%s, %s" key @< pps_env xs

let rec map env f =
  match env with
    [] -> []
  | (k,v)::t -> (k, f v)::(map t f)

let rec fold env f x = List.fold_left f x env

let rec remove env x = List.remove_assoc x env

(* if there are same variables between env and env', the value is in env' *)
let extend_by_env env env' =
  extendl env @< list_of env'

let members env =
  fold env (fun acc (k, _) -> VariableSet.add k acc) VariableSet.empty 
