let (@<) = fun x y -> x y
let (+>) = fun x y -> y x
let ($<)  = fun f g x -> f (g x)
