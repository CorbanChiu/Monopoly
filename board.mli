(** The abstract type of values representing a board*)
type board

(** The abstract type of value representing any given square*)
type square

type property

(** The type of value representing a color (r,g,b) *)
type propertycolor = int * int * int

exception UnknownJSON

val from_json : Yojson.Basic.t -> board

(** [get_square b n] is a square that is in the nth position of b*)
val get_square : board -> int -> square

val find_square : board -> square -> int

val namelist : board -> string list

val pricelist : board -> int option list

val colorlist : board -> propertycolor option list

(* TODO: is this really necessary to have here? *)
val mortgagelist : board -> int option list

val mortgage : square -> int option

(** [propertygroup b sq] is the square list of squares part of the same
    "color grouping" (that which you need to own all properties to
    build) *)
val propertygroup : board -> square -> square list

val railroadgroup : board -> square list

val utilitygroup : board -> square list

val get_name : board -> square -> string

val init_prop_lst : board -> property list

val update_property_new_owner : property -> string -> property
