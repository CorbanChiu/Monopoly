(** [property] is a board type *)
type property = Board.property

(* The type of value that represents a card (either community chest or
   chance) *)
type card

(* [game_state] represents the state of the game at a specific moment in
   time *)
type game_state = {
  property_lst : (int * property) list;
  player_lst : (int * Player.player) list;
  next : int;
  cards : card;
}

(** [get_property ind lst] returns the property associated with the
    index ind in property list lst*)
val get_property : int -> (int * property) list -> property

(** [update_propertylst ind np lst] returns a property list lst where
    the index ind contains the updated property np. *)
val update_property_lst :
  int -> property -> (int * property) list -> (int * property) list

(** [get_player index player_list] returns the player type associated
    with the given index in player list player_list *)
val get_player : int -> (int * Player.player) list -> Player.player

(** [update_player_lst ind np lst] returns player list lst where the
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

(*( [get_player_jail_state gs i] returns true iff player i is in jail
  during gamestate gs) *)
val get_player_jail_state : game_state -> int -> bool

(** [get_square_owner gs ind] returns the option owner of the square
    associated with index ind in game_state association property list. *)
val get_square_owner : game_state -> int -> string option

(** [get_square_dev_lvl gs ind] returns the option development level of
    the square associated with index ind in game_state association
    property list. *)
val get_square_dev_lvl : game_state -> int -> int option

(** [get_square_mortgage_state gs ind] returns the option bool "if
    mortgaged" of the square associated with index ind in game_state
    association property list. *)
val get_square_mortgage_state : game_state -> int -> bool option

(** [init_game_state names] returns the initial game state which
    represents the start of the game with players initialized to the
    values in the names namelist*)
val init_game_state : string list -> game_state

(** [go_to_jail gs pl] returns a new gamestate with player pl in jail *)
val go_to_jail : game_state -> Player.player -> game_state

(** [move gs dr] returns a new game state after changing the position of
    the player whose turn it is, based on the diceroll dr *)
val move : game_state -> int * int -> game_state

(** [current_player gs] returns the player whose turn it currently is *)
val current_player : game_state -> Player.player

(** [roll_dice] returns a random integer between 2 and 12 (inclusive). *)
val roll_dice : unit -> int * int

(** [current_turn_name gs] returns the name of the player whose turn it
    is *)
val current_turn_name : game_state -> string option

(** [buy_property gs] executes the buying of the property a player has
    landed on and returns a new gamestate *)
val buy_property : game_state -> game_state

(** [add_rent gs dr] adds rent (as debt) corresponding to gs and the
    diceroll, dr, and returns a new gamestate *)
val add_rent : game_state -> int -> game_state

(** [add_luxury gs] adds debt corresponding to the payment of luxury tax
    in gs, and returns a new gamestate *)
val add_luxury_tax : game_state -> game_state

(** [add_income_tax gs] adds debt corresponding to the payment of income
    tax in gs, and returns a new gamestate *)
val add_income_tax : game_state -> game_state

(** [pay gs] attempts to pay off all of the current player's debt, and
    returns the corresponding new gamestate *)
val pay : game_state -> game_state

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

(** [cards gs] processes the action of the card landed on by the current
    player and returns the corresponding new gamestate *)
val cards : game_state -> game_state

(** [on_cc gs dr] is true iff the current player would be on a community
    chest square following the diceroll dr *)
val on_cc : game_state -> (int * int) option -> bool

(** [on_chance gs dr] is true iff the current player would be on a
    chance square following the diceroll dr *)
val on_chance : game_state -> (int * int) option -> bool

(** [get_cc_pile gs] returns the community chest cardpile,
    stacked/sorted as it is during the gamestate gs *)
val get_cc_pile : game_state -> Cards.cardpile

(** [get_cc_pile gs] returns the chance cardpile, stacked/sorted as it
    is during the gamestate gs *)
val get_chance_pile : game_state -> Cards.cardpile

(** [free_parking gs] returns the amount of money in free parking during
    the gamestate gs *)
val free_parking : game_state -> int

(** [good_output gs] returns a new gamestate with the property list and
    player list sorted based on key integer values *)
val good_output : game_state -> game_state

(** [assoc_list_length lst] returns the length of the list lst *)
val assoc_list_length : 'a list -> int

(** [end_turn gs] ends current players turn and returns a new gamestate *)
val end_turn : game_state -> game_state

(** [can_buy_property gs] returns true if the current player is able to
    buy the property they have landed on *)
val can_buy_property : game_state -> bool

(** [can_pay_luxury gs] returns true iff the current player is able to
    pay luxury tax (and is on the luxury tax square) *)
val can_pay_luxury : game_state -> bool

(** [can_pay_income gs] returns true iff the current player is able to
    pay income tax (and is on the income tax square) *)
val can_pay_income : game_state -> bool

(** [can_pay_rent gs] returns true if the current player is able to pay
    the rent for the property they have landed on *)
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

(** [test_game_state gs] returns a testing gamestate which represents a
    scenario in the middle of a game for testing purposes *)
val test_game_state : game_state

(** [demo_state gs] returns a demo gamestate which represents a scenario
    in the middle of a game for demoing purposes *)
val demo_game_state : game_state
