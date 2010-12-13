module M = Map.Make(struct
                      type t = string
                      let compare = String.compare
                    end)

type t = (int * int M.t)

let empty = (-1, M.empty)
  
let add s (i, map) =
  if M.mem s map then (i, map)
  else begin
    let n = i + 1 in
    (n, M.add s n map)
  end
let refresh (i, _) = (i, M.empty)

let mem k (_, map)  = M.mem k map
let find k (_, map) = M.find k map
