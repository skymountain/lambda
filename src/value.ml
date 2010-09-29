open Misc
open Syntax

type t =
    IntV  of int
  | BoolV of bool
  | FunV  of id * exp * (id * t) Env.t ref
  | ListV of t list
      
(* pretty printer for value *)
let rec pps_val = function
    IntV i   -> string_of_int i
  | BoolV b  -> string_of_bool b
  | FunV _   -> "<fun>"
  | ListV vs -> Printf.sprintf "[%s]" @< String.concat "; " @< List.map pps_val vs
      
let pp_val v =
  print_string @< pps_val v
