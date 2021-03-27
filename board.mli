(** The abstract type of values representing a board*)
type board

(** The abstract type of value representing any given square*)
type square

(** The type of value representing the development level of any given
    square as an integer from 0 to 5, zero signifying the absence of
    houses, 1-4 signifying the amount of houses, and 5 signifying the
    presence of a hotel*)

(** [get_square b n] is a square that is in the nth position of b*)
val get_square : board -> int -> square

val namelist : board -> string list

val pricelist : board -> int list

val colorlist : board -> (int * int * int) list

(** [developlist b] returns an int list where each int representing the
    development level of the corresponding square of board [b] as an
    integer from 0 to 5, zero signifying the absence of houses, 1-4
    signifying the amount of houses, and 5 signifying the presence of a
    hotel*)
val developlist : board -> int list
