type property = Board.property

type card

type game_state

(* (* = { property_lst : (int * property) list; player_lst : (int *
   Player.player) list; next : int; *) } *)

val get_property : int -> (int * property) list -> property

val update_property_lst :
  int -> property -> (int * property) list -> (int * property) list

val get_player : int -> (int * Player.player) list -> Player.player

val update_player_lst :
  int ->
  Player.player ->
  (int * Player.player) list ->
  (int * Player.player) list

val get_players_name : game_state -> (int * string option) list

val get_players_position : game_state -> (int * int) list

val get_players_cash : game_state -> (int * int) list

val get_player_jail_state : game_state -> int -> bool

val get_square_owner : game_state -> int -> string option

val get_square_dev_lvl : game_state -> int -> int option

val get_square_mortgage_state : game_state -> int -> bool option

val init_game_state : string list -> game_state

val go_to_jail : game_state -> Player.player -> game_state

val move : game_state -> int * int -> game_state

val current_player : game_state -> Player.player

val roll_dice : unit -> int * int

val current_turn_name : game_state -> string option

(* val move : game_state -> int -> game_state *)

val buy_property : game_state -> game_state

val add_rent : game_state -> int -> game_state

val add_luxury_tax : game_state -> game_state

val add_income_tax : game_state -> game_state

val pay : game_state -> game_state

val mortgage : game_state -> int -> game_state

val unmortgage : game_state -> int -> game_state

val develop_property : game_state -> int -> game_state

val undevelop_property : game_state -> int -> game_state

val cards : game_state -> game_state

val on_cc : game_state -> bool

val on_chance : game_state -> bool

val get_cc_pile : game_state -> Cards.cardpile

val get_chance_pile : game_state -> Cards.cardpile

val good_output : game_state -> game_state

val assoc_list_length : 'a list -> int

val end_turn : game_state -> game_state

val can_buy_property : game_state -> bool

val can_pay_luxury : game_state -> bool

val can_pay_income : game_state -> bool

val can_pay_rent : game_state -> int -> bool

val can_mortgage : game_state -> int -> bool

val can_unmortgage : game_state -> int -> bool

val can_develop_property : game_state -> int -> bool

val can_undevelop_property : game_state -> int -> bool

val demo_game_state : game_state
