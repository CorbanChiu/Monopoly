(** The abstract type of values representing a board*)
type board

(** The abstract type of value representing any given square*)
type square

(** The abstract type of value representing a color *)
type propertycolor

val from_json : Yojson.Basic.t -> board

(** [get_square b n] is a square that is in the nth position of b*)
val get_square : board -> int -> square

val namelist : board -> string list

val pricelist : board -> int option list

val colorlist : board -> propertycolor option list

(* TODO: is this really necessary to have here? *)
val mortgagelist : board -> int option list

(** [propertygroup b sq] is the square list of squares part of the same
    "color grouping" (that which you need to own all properties to
    build) *)
val propertygroup : board -> square -> square list
