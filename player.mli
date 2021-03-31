type player

val position : player -> int

val cash : player -> int

val properties : player -> Board.square list option

val cards : player -> Cards.card list option

val jail : player -> bool

val token : player -> Token.token option

val name : player -> string option

val net_worth : player -> int

val bankrupt : player -> bool
