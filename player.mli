type player

val get_name : player -> string option

val update_name : player -> string option -> player

val get_token : player -> Token.token option

val update_token : player -> Token.token option -> player

val get_position : player -> int

val update_position : player -> int -> player

val get_cash : player -> int

val increment_cash : player -> int -> player

val decrement_cash : player -> int -> player

val get_property_lst : player -> Board.property list

val add_property : Board.property -> player -> player

val remove_property : player -> Board.property -> player

val get_card_lst : player -> Cards.card list

val add_card : player -> Cards.card -> player

val remove_card : player -> Cards.card -> player

val get_jail_state : player -> bool

val update_jail_state : player -> bool -> player

val get_bankrupt_state : player -> bool

val update_bankrupt_state : player -> bool -> player

val get_player_from_name :
  (int * player) list -> string option -> player

(* val net_worth : player -> int *)

(* val bankrupt : player -> bool *)

val move : player -> int -> player

val init_player : player
