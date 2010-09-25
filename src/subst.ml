open Misc
open Syntax
open Common
open OptionMonad

type t = typvar * typ list
type eq = typ * typ

let empty = []
let equations_of subst = List.fold_left (fun acc (id, typ) -> (TypVar id, typ)::acc) [] subst

let subst_in_equations id typ =
  let rec iter src_typ =
    match src_typ with
      BoolT | IntT -> src_typ
    | FunT (ftyp, rtyp) -> FunT (iter ftyp, iter rtyp)
    | TypVar id' -> begin
        if id = id' then typ
        else src_typ
      end
  in
  List.map (fun (typ1, typ2) -> (iter typ1, iter typ2))

let invalid_eq =
  let rec contain_typvar id = function
      IntT | BoolT -> false
    | TypVar x -> id = x
    | FunT (ftyp, rtyp) -> contain_typvar id ftyp || contain_typvar id rtyp
  in
  fun id typ ->
    match typ with
      TypVar id' when id = id' -> false
    | _ -> contain_typvar id typ
    
let unifyl subst eqs =
  let rec iter acc = function
      [] -> Some acc
    | eq::eqs -> begin
        match eq with
          IntT, IntT | BoolT, BoolT ->
            iter acc eqs
              
        | TypVar id, (_ as typ) | (_ as typ), TypVar id -> begin
            if invalid_eq id typ then None
            else
              let acc = (id, typ)::acc in
              let eqs = subst_in_equations id typ eqs in
              iter acc eqs
          end
            
        | FunT (ftyp1, rtyp1), FunT (ftyp2, rtyp2) ->
            iter acc @< (ftyp1, ftyp2)::(rtyp1, rtyp2)::eqs
              
        | (IntT|BoolT|FunT _), _ -> None
      end
  in
  (iter empty @< eqs @ (equations_of subst)) >>= (fun l -> Some (List.rev l))

let unify subst eq =
  unifyl subst [eq]

let make_eq typ1 typ2 = (typ1, typ2)
let make_eqs eqs : (typ * typ) list = eqs

let rec subst_typvar subst id =
  let rec subst_fun src_typ id dst_typ eqs =
    match src_typ with
      IntT | BoolT -> src_typ
    | TypVar id' -> begin
        if id = id' then body dst_typ eqs
        else body src_typ eqs
      end
    | FunT (ftyp, rtyp) ->
        FunT (subst_fun ftyp id dst_typ eqs, subst_fun rtyp id dst_typ eqs)
          
  and body typ = function
      [] -> typ
    | (id, dst_typ)::eqs ->
        subst_fun typ id dst_typ eqs
  in
  body (TypVar id) subst

let rec subst_typ subst = function
    IntT -> IntT
  | BoolT -> BoolT
  | FunT (ftyp, rtyp) -> FunT (subst_typ subst ftyp, subst_typ subst rtyp)
  | TypVar id -> subst_typvar subst id

let rec subst_type_scheme subst typ_scheme =
   let bound_typvars = TypeScheme.bound_typvars typ_scheme in
   let typ = TypeScheme.typ typ_scheme in
   let rec iter = function
       IntT -> IntT
     | BoolT -> BoolT
     | FunT (ftyp, rtyp) -> FunT (iter ftyp, iter rtyp)
     | TypVar id -> begin
         if TypVarSet.mem id bound_typvars then TypVar id
         else subst_typvar subst id
       end
   in
   (bound_typvars, iter typ)
     
let rec subst_tenv subst tenv =
  Env.fold
    (fun acc (var, typ_scheme) ->
       Env.extend acc var @< subst_type_scheme subst typ_scheme)
    Env.empty tenv
