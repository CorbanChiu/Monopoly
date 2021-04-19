type player

val init_player : player

val position : player -> int

val cash : player -> int

val properties : player -> Board.square list

val cards : player -> Cards.card list

val jail : player -> bool

val token : player -> Token.token option

val name : player -> string option

val net_worth : player -> int

val bankrupt : player -> bool

val move : player -> int -> player

val add_property : player -> Board.square -> player
