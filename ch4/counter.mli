open Base

(** A collection of string frequency counts*)
type t

(** The empty set of frequency counts*)
val empty : t

(** Bump the frequency count for the given string*)
val touch : t -> string -> t

(** Converts the set of frequency counts to an association list.
    A string shows up at most once, and the counts are >= 1*)
val to_list : t -> (string * int) list

(** Represents the median computed from a set of strings.
    In the case where there is an even number of choices, the one before and after
    the median are returned. *)
type median =
  | Median of string
  | Before_and_after of string * string

val median : t -> median
