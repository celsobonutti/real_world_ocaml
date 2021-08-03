let apply_to_tuple f (first, second) = f ~first ~second

let apply_to_tuple_2 f (first, second) = f ~second ~first

let divide ~first ~second = first / second

let value = apply_to_tuple divide (2, 2)

let concat ?sep x y =
  let sep = match sep with None -> "" | Some x -> x in
  x ^ sep ^ y

let concat ?(sep="") x y = x ^ sep ^ y

let _ = concat ?sep:(Some ":") "foo" "bar"

let uppercase_concat ?sep a b = concat ?sep (String.uppercase_ascii a) b
