(** [property] is a board type *)
type property = Board.property

type card

type game_state = {
  property_lst : (int * property) list;
  player_lst : (int * Player.player) list;
  next : int;
  cards : card;
}

(* (* = { property_lst : (int * property) list; player_lst : (int *
   Player.player) list; next : int; *) } *)

(** [get_property ind lst] returns the property associated with the
    index ind*)
val get_property : int -> (int * property) list -> property

(** [updated_propertylst ind np lst] returns a property list lst where
    the index ind contains the updated property np. *)
val update_property_lst :
  int -> property -> (int * property) list -> (int * property) list

(** [get_player index player_list] returns the player type associated
    with the given index. *)
val get_player : int -> (int * Player.player) list -> Player.player

(** [updated_playerlst ind np lst] returns a player list lst where the
    index ind contains the updated player np. *)
val update_player_lst :
  int ->
  Player.player ->
  (int * Player.player) list ->
  (int * Player.player) list

(** [get_players_name gs] returns a player list with the player type
    replaced with the player name. *)
val get_players_name : game_state -> (int * string option) list

(** [get_players_position gs] returns a player list with the player type
    replaced with the player position on the board. *)
val get_players_position : game_state -> (int * int) list

(** [get_players_cash gs] returns a player list with the player type
    replaced with the player cash value. *)
val get_players_cash : game_state -> (int * int) list

(** [get_square_owner gs ind] returns a property list with the property
    type replaced with the property square owner. *)
val get_square_owner : game_state -> int -> string option

(** [get_square_owner gs ind] returns a property list with the property
    type replaced with the property development level. *)
val get_square_dev_lvl : game_state -> int -> int option

(** [get_square_owner gs ind] returns a property list with the property
    type replaced with the property mortgage state. *)
val get_square_mortgage_state : game_state -> int -> bool option

(** [init_game_state names] returns the initial game state which
    represents the start of the game with players initialized to the
    values in the names list*)
val init_game_state : string list -> game_state

val go_to_jail : game_state -> Player.player -> game_state

(** [move gs dr] returns a new game state gs after changing the position
    of the player whose turn it is, based on the diceroll dr *)
val move : game_state -> int * int -> game_state

val current_player : game_state -> Player.player

(** [roll_dice] returns a random integer between 2 and 12 (inclusive). *)
val roll_dice : unit -> int * int

(** [current_turn_name gs] returns the name of the player whose turn it
    is *)
val current_turn_name : game_state -> string option

(* val move : game_state -> int -> game_state *)

(** [buy_property gs] executes the buying of the property a player has
    landed on and returns a new gamestate *)
val buy_property : game_state -> game_state

(** [pay_rent gs] executes paying rent for the property a player has
    landed on and returns a new gamestate *)
val pay_rent : game_state -> int -> game_state

(** [mortgage gs] executes mortgaging of a property a player owns and
    returns a new gamestate *)
val mortgage : game_state -> int -> game_state

(** [unmortgage gs] executes unmortgaging of a property a player owns
    and returns a new gamestate *)
val unmortgage : game_state -> int -> game_state

(** [develop_property gs] executes development of a property a player
    owns and returns a new gamestate *)
val develop_property : game_state -> int -> game_state

(** [undevelop_property gs] executes undevelopment of a property a
    player owns and returns a new gamestate *)
val undevelop_property : game_state -> int -> game_state

(** [good_output gs] returns a new gamestate with the property list and
    player list sorted based on key integer values *)
val good_output : game_state -> game_state

(** [assoc_list_length gs] returns the length of the list lst *)
val assoc_list_length : 'a list -> int

(** [end_turn gs] ends current players turn and returns a new gamestate *)
val end_turn : game_state -> game_state

(** [can_buy_property gs] returns true if the current player is able to
    buy the property they have landed on *)
val can_buy_property : game_state -> bool

(** [can_pay_rent gs] returns true if the current player is able to buy
    the property they have landed on *)
val can_pay_rent : game_state -> int -> bool

(** [can_mortgage gs] returns true if the current player can mortgage a
    selected property *)
val can_mortgage : game_state -> int -> bool

(** [can_unmortgage gs] returns true if the current player can
    unmortgage the selected property *)
val can_unmortgage : game_state -> int -> bool

(** [can_develop_property gs] returns true if the current player can
    develop a selected property *)
val can_develop_property : game_state -> int -> bool

(** [can_undevelop_property gs] returns true if the current player can
    undevelop a selected property *)
val can_undevelop_property : game_state -> int -> bool

(** [demo_state gs] returns a demo gamestate which represents a scenario
    in the middle of a game for demoing purposes *)
val demo_game_state : game_state
