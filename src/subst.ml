open Misc
open Syntax
open Common
open OptionMonad

type t = typvar * typ list
type eq = typ * typ

let empty = []

(* substitution *)
let rec subst_typvar subst id =
  let rec subst_per_eq src_typ id dst_typ eqs =
    match src_typ with
      IntT | BoolT -> src_typ
    | TypVar id' -> begin
        if id = id' then body dst_typ eqs
        else body src_typ eqs
      end
    | FunT (ftyp, rtyp) ->
        FunT (subst_per_eq ftyp id dst_typ eqs, subst_per_eq rtyp id dst_typ eqs)
    | ListT etyp -> ListT (subst_per_eq etyp id dst_typ eqs)
          
  and body typ = function
      [] -> typ
    | (id, dst_typ)::eqs ->
        subst_per_eq typ id dst_typ eqs
  in
  body (TypVar id) subst

let rec subst_typ subst = function
    IntT -> IntT
  | BoolT -> BoolT
  | FunT (ftyp, rtyp) -> FunT (subst_typ subst ftyp, subst_typ subst rtyp)
  | TypVar id -> subst_typvar subst id
  | ListT etyp -> ListT (subst_typ subst etyp)
      
let rec subst_type_scheme subst typ_scheme =
   let bound_typvars = TypeScheme.bound_typvars typ_scheme in
   let typ = TypeScheme.typ typ_scheme in
   let rec iter = function
       IntT -> IntT
     | BoolT -> BoolT
     | FunT (ftyp, rtyp) -> FunT (iter ftyp, iter rtyp)
     | ListT etyp -> ListT (iter etyp)
     | TypVar id -> begin
         if TypVarSet.mem id bound_typvars then TypVar id
         else subst_typvar subst id
       end
   in
   (bound_typvars, iter typ)
     
let rec subst_tenv subst tenv =
  Env.fold tenv 
    (fun acc (var, typ_scheme) ->
       Env.extend acc var @< subst_type_scheme subst typ_scheme)
    Env.empty

(* unify *)
let equations_of subst = List.fold_left (fun acc (id, typ) -> (TypVar id, typ)::acc) [] subst

let subst_in_equations id typ =
  let rec iter src_typ =
    match src_typ with
      BoolT | IntT -> src_typ
    | FunT (ftyp, rtyp) -> FunT (iter ftyp, iter rtyp)
    | ListT etyp -> ListT (iter etyp)
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
    | ListT etyp -> contain_typvar id etyp
  in
  fun id typ ->
    match typ with
      TypVar id' when id = id' -> false
    | _ -> contain_typvar id typ

let unify_eqs eqs =
  let rec unify_eq acc eqs = function
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
    | ListT etyp1, ListT etyp2 -> iter acc @< (etyp1, etyp2)::eqs
        
    | (IntT|BoolT|FunT _|ListT _), _ -> None

  and iter acc = function
      [] -> Some acc
    | eq::eqs -> unify_eq acc eqs eq
  in
  (iter empty eqs) >>= fun l -> Some (List.rev l)
  
let unifyl subst eqs =
  unify_eqs @< eqs @ (equations_of subst)
  (* (iter empty @< eqs @ (equations_of subst)) >>= (fun l -> Some (List.rev l)) *)

let unify subst typ1 typ2 =
  unifyl subst [(typ1, typ2)]

let unify_list subst typ =
  let etyp = Type.fresh_typvar () in
  let ltyp = ListT etyp in
  unify subst typ ltyp >>= fun subst -> Some (subst, subst_typ subst etyp)
    
let merge subst1 subst2 = 
  unifyl subst1 @< equations_of subst2
  
