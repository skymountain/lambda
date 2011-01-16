let (@<) = fun x y -> x y
let (+>) = fun x y -> y x
let ($<)  = fun f g x -> f (g x)

let make_list i x =
  let rec iter ii acc =
    if ii = 0 then acc
    else iter (ii-1) (x::acc)
  in
  iter i []
