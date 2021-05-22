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

val get_property_lst : player -> int list

val add_property : int -> player -> player

val remove_property : player -> int -> player

val get_card_lst : player -> Cards.card list

val add_card : player -> Cards.card -> player

val remove_card : player -> Cards.card -> player

val get_jail_state : player -> int

val update_jail_state : player -> int -> player

val get_bankrupt_state : player -> bool

val update_bankrupt_state : player -> bool -> player

val get_in_debt : player -> (int * int) list

val update_in_debt : player -> (int * int) list -> player

val add_debt : player -> int -> int -> player

val remove_debt : player -> int -> int -> player

val total_debt : player -> int

val no_debt : player -> bool

val get_debt : player -> int -> int

val get_player_from_name :
  (int * player) list -> string option -> player

(* val net_worth : player -> int *)

(* val bankrupt : player -> bool *)

val init_player : player

val get_gojf : player -> int

val add_gojf : player -> player

val remove_gojf : player -> player

val incr_net_worth : int -> player -> player

val decr_net_worth : int -> player -> player
