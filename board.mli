(** The abstract type of values representing a board*)
type board

(** The abstract type of value representing any given square*)
type square

(* TODO: is this necessary? removing this will also necessitate the
   removal of get_paymentstruct from the mli *)
type paymentstruct

(** The type of value representing a color (r,g,b) *)
type propertycolor = int * int * int

type paymentstructure = (int * int) list option

exception UnknownJSON

(** [from_json] initializes the board by unpacking the json into board
    types*)
val from_json : Yojson.Basic.t -> board

(** [get_square b n] returns a square that is in the nth position of b*)
val get_square : board -> int -> square

(** [find_square b s] returns index of square s in board square list b*)
val find_square : board -> square -> int

(** [get_name_from_square s] returns name of square s *)
val get_name_from_square : square -> string

(** [namelist lst] returns board square list lst with each square
    replaced with its name *)
val namelist : board -> string list

(** [get_price s] returns price of square s *)
val get_price : square -> int option

(** [pricelist lst] returns board square list lst with each square
    replaced with its price *)
val pricelist : board -> int option list

(** [get_payments s] returns paymentstructure of square s *)
val get_payments : square -> paymentstructure

(** [get_color s] returns propertycolor of square s *)
val get_color : square -> propertycolor option

(** [colorlist b] returns propertycolor of square s *)
val colorlist : board -> propertycolor option list

(** [get_mortgage s] returns mortgage price of square s *)
val get_mortgage : square -> int option

(** [mortgagelist lst] returns board square list lst with each square
    replaced with its mortgage attribute *)
val mortgagelist : board -> int option list

(** [get_buildprice s] returns build price of square s *)
val get_buildprice : square -> int option

(** [get_mortgage s] returns "if mortgaged" of square s *)
val get_mortgage : square -> int option

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

val check_no_development : property -> property list -> bool

val check_equal_development : property -> property list -> bool

val check_equal_undevelopment : property -> property list -> bool

val check_no_mortgages : property -> property list -> bool

type action =
  | Buy_ok
  | Payrent_ok
  | Mortgage_ok
  | Mortgage_and_Develop_ok
  | Unmortgage_ok
  | Develop_and_Undevelop_ok
  | Undevelop_ok
  | Chance_ok
  | CC_ok
  | Freeparking_ok
  | None_ok
  | Gotojail_ok
  | Auction_ok
  | Go_ok
  | Incometax_ok
  | Luxurytax_ok

val get_action : property -> string option -> action
