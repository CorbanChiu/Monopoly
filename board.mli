(** The abstract type of values representing a board*)
type board

(** The abstract type of value representing any given square*)
type square

(** [get_square b n] is a square that is in the nth position of b*)
val get_square : board -> int -> square
