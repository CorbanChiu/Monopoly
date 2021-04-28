type player

val init_player : player

val position : player -> int

val move : player -> int -> player

val cash : player -> int

val properties : player -> Board.square list

val cards : player -> Cards.card list

val jail : player -> bool

val token : player -> Token.token option

val name : player -> string option

val net_worth : player -> int

val bankrupt : player -> bool

val move : player -> int -> player

val increment_cash : player -> int -> player

val decrement_cash : player -> int -> player

val add_property : player -> Board.square -> player

val remove_property : player -> Board.square -> player

val add_card : player -> Cards.card -> player

val remove_card : player -> Cards.card -> player

val send_to_jail : player -> player

val let_out_of_jail : player -> player

val change_to_bankrupt : player -> player

val change_to_not_bankrupt : player -> player

val get_player_from_player_list_given_name :
  ('a * player) list -> string option -> player

val get_player_number : ('a * player) list -> player -> 'a
