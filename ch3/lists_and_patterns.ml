open Base
open Base.Poly


let _ = [1;2;3]

let _ = 1 :: 2 :: 3 :: 4 :: []

let rec sum = function
  | [] -> 0
  | hd :: tl -> hd + sum tl

let rec drop_value to_drop = function
  | [] -> []
  | hd :: tl when hd = to_drop -> drop_value to_drop tl
  | hd :: tl -> hd :: drop_value to_drop tl

let max_widths header rows =
  let lengths l = List.map ~f:String.length l in
  List.fold rows
    ~init:(lengths header)
    ~f:(fun acc row ->
      List.map2_exn ~f:Int.max acc (lengths row))

let render_separator widths =
  let pieces = List.map widths
      ~f:(fun w -> String.make (w + 2) '-')
  in
  "|" ^ String.concat ~sep:"+" pieces ^ "|"

let pad s length =
  " " ^ s ^ String.make (length - String.length s + 1) ' '

let render_row row widths =
  let padded = List.map2_exn row widths ~f:pad in
  "|" ^ String.concat ~sep:"|" padded ^ "|"

let render_table header rows =
  let widths = max_widths header rows in
  String.concat ~sep:"\n"
    (render_row header widths
     :: render_separator widths
     :: List.map rows ~f:(fun row -> render_row row widths))

let rec destutter = function
  | [] | [_] as l -> l
  | hd :: (hd' :: _ as tl) when hd = hd' -> destutter tl
  | hd :: tl -> hd ::destutter tl
