(** Responsible for reading a monopoly board from JSON, querying values
    of this board, and changing attributes of the board's properties *)

(** The abstract type of values representing a board*)
type board

(** The abstract type of value representing any given square*)
type square

(* TODO: is this necessary? removing this will also necessitate the
   removal of get_paymentstruct from the mli *)

(** The option paymentstructure *)
type paymentstruct

(** The abstract type of value representing a color (r,g,b) *)
type propertycolor = int * int * int

(** The abstract type of value representing the various rent prices for
    a given property at different stages of development *)
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

(** [propertygroup b sq] returns the square list of squares part of the
    same "color grouping" (that which you need to own all properties to
    build) *)
val propertygroup : board -> square -> square list

(** [railroadgroup b] returns the square list representing all railroad
    squares *)
val railroadgroup : board -> square list

(** [utilitygroup b] returns the square list representing all utility
    squares *)
val utilitygroup : board -> square list

(** [get_name_from_board b s] returns the name of square s in board b *)
val get_name_from_board : board -> square -> string

(** The abstract type of value that represents a property (which can be
    bought, developed etc)*)
type property

(** [get_name_from_board p] returns the square of property p *)
val get_sqr : property -> square

(** [update_sqr p s] returns property p with its square attribute set to
    square s*)
val update_sqr : property -> square -> property

(** [get_owner p] returns the owner of property p *)
val get_owner : property -> string option

(** [update_owner p n] returns property p with its owner attribute set
    to name option n*)
val update_owner : property -> string option -> property

(** [get_dev_lvl p] returns the development level of property p *)
val get_dev_lvl : property -> int option

(** [update_dev_lvl p i] returns property p with its development level
    attribute set to level option i*)
val update_dev_lvl : property -> int option -> property

(** [get_mortgage_state p] returns "if property p is mortgaged" *)
val get_mortgage_state : property -> bool option

(** [update_mortgage_state p b] returns property p with its mortgage
    state attribute set to bool option b*)
val update_mortgage_state : property -> bool option -> property

(** [remove_option op] returns the value carried by option op *)
val remove_option : 'a option -> 'a

(** [init_prop_lst b i] returns the starting game (index * property)
    association list which represents the layout of properties around
    the board*)
val init_prop_lst : board -> int -> (int * property) list

(* val num_color_group : propertycolor option -> board -> int *)

(** [complete_propertygroup p sqlist b] returns true if a player
    posseses owns the whole color group that property p is a part of*)
val complete_propertygroup : property -> square list -> board -> bool

(** [get_rent p sqlist b i] returns the rent price a player must pay
    based on the property the player has landed on. Diceroll i may act
    as a mulitplier in specific cases*)
val get_rent : property -> square list -> board -> int -> int

(** [check_no_development p plst] returns true if all properties of the
    same color group as property p is 0 (this included property p)*)
val check_no_development : property -> property list -> bool

(** [check_equal_development p plst] returns true if all properties of
    the same color group as property p are at most 1 development level
    greater than property p development level *)
val check_equal_development : property -> property list -> bool

(** [check_equal_undevelopment p plst] returns true if all properties of
    the same color group as property p are at least 1 development level
    less than property p development level *)
val check_equal_undevelopment : property -> property list -> bool

(** [check_no_mortgages p plst] returns true if all properties of the
    same color group as property p are unmortgaged (this includes
    property p) *)
val check_no_mortgages : property -> property list -> bool

(** The abstract type of value representing the different possible
    actions a given player can make during the game*)
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

(** [get_action p pl] returns the variant of the action type which
    represents if that specific action is legal at that point in the
    game*)
val get_action : property -> string option -> action
