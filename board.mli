(** The abstract type of values representing a board*)
type board

(** The abstract type of value representing any given square*)
type square

(* TODO: is this necessary? removing this will also necessitate the
   removal of get_paymentstruct from the mli *)
type paymentstruct

(** The type of value representing a color (r,g,b) *)
type propertycolor = int * int * int

exception UnknownJSON

val from_json : Yojson.Basic.t -> board

(** [get_square b n] is a square that is in the nth position of b*)
val get_square : board -> int -> square

val find_square : board -> square -> int

val get_name_from_square : square -> string

val namelist : board -> string list

val get_price : square -> int option

val pricelist : board -> int option list

val get_paymentstruct : square -> paymentstruct option

val get_color : square -> propertycolor option

val colorlist : board -> propertycolor option list

val get_mortgage : square -> int option

(* TODO: is this really necessary to have here? *)
val mortgagelist : board -> int option list

val get_buildingcost : square -> int option

(** [propertygroup b sq] is the square list of squares part of the same
    "color grouping" (that which you need to own all properties to
    build) *)
val propertygroup : board -> square -> square list

val railroadgroup : board -> square list

val utilitygroup : board -> square list

val get_name_from_board : board -> square -> string

type property

val get_sqr : property -> square

val update_sqr : property -> square -> property

val get_owner : property -> string option

val update_owner : property -> string option -> property

val get_dev_lvl : property -> int option

val update_dev_lvl : property -> int option -> property

val get_mortgage_state : property -> bool option

val update_mortgage_state : property -> bool option -> property

val remove_option : 'a option -> 'a

val init_prop_lst : board -> int -> (int * property) list

(* val num_color_group : propertycolor option -> board -> int *)

val complete_propertygroup : property -> square list -> board -> bool

val get_rent : property -> square list -> board -> int -> int

val check_equal_development : property -> property list -> bool

val check_no_mortgages : property -> property list -> bool

type action =
  | Buy_ok
  | Auction_ok
  | Payrent_ok
  | Mortgage_ok
  | Card_ok
  | Freeparking_ok
  | None
  | Gotojail_ok
  | Go_ok
  | Incometax_ok
  | Luxurytax_ok
