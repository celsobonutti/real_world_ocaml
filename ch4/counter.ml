open Base

type t = (string, int, String.comparator_witness) Map.t

let empty = Map.empty (module String)

let to_list t = Map.to_alist t

let touch counts line =
  Map.update counts line ~f:(function
    | None -> 1
    | Some x -> x + 1)

type median =
  | Median of string
  | Before_and_after of string * string

let median t =
  let sorted_strings =
    List.sort (Map.to_alist t)
      ~compare:(fun (_, x) (_, y) -> Int.descending x y)
  in
  let len = List.length sorted_strings in
  if len = 0 then failwith "median: empty frequency count";
  let nth n = fst (List.nth_exn sorted_strings n) in
  if len % 2 = 1
  then Median (nth (len/2))
  else Before_and_after (nth (len/2 - 1), nth (len/2))
