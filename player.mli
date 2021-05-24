(** [player] represents players of the game *)
type player

(** [get_name pl] returns the name of the player pl*)
val get_name : player -> string option

(** [update_name pl s] returns player pl with an updated name s *)
val update_name : player -> string option -> player

(** [get_token pl] returns token of player pl *)
val get_token : player -> Token.token option

(** [update_token pl t] returns player pl with token updated with t*)
val update_token : player -> Token.token option -> player

(** [get_position pl] returns position of player pl *)
val get_position : player -> int

(** [update_position pl i] returns player pl with position updated to i *)
val update_position : player -> int -> player

(** [get_cash pl] returns cash of player pl *)
val get_cash : player -> int

(** [increment_cash pl v] returns player pl with cash incremented by v *)
val increment_cash : player -> int -> player

(** [decrement_cash pl v] returns player pl with cash decremented by v *)
val decrement_cash : player -> int -> player

(** [get_property_lst pl] returns property list of player pl *)
val get_property_lst : player -> int list

(** [get_networth pl] returns networth of player pl *)
val get_networth : player -> int

(** [add_property i pl] returns player pl with property at index i
    (based on the board) added to the players property list *)
val add_property : int -> player -> player

(** [remove_property pl i] returns player pl with property at index i
    (based on the board) removed from the players property list *)
val remove_property : player -> int -> player

(** [get_card_lst pl] returns card list of player pl *)
val get_card_lst : player -> Cards.card list

(** [add_card pl c] returns player pl with card c added to the players
    card list *)
val add_card : player -> Cards.card -> player

(** [remove_card pl c] returns player pl with card c removed from the
    players card list *)
val remove_card : player -> Cards.card -> player

(** [get_jail_state pl] returns jail state of player pl *)
val get_jail_state : player -> int

(** [update_jail_state pl i] returns player pl with jail state changed
    to i *)
val update_jail_state : player -> int -> player

(** [get_bankrupt_state pl] returns bankrupt state of player pl *)
val get_bankrupt_state : player -> bool

(** [update_bankrupt_state pl b] returns player pl with bankrupt state
    updated to b *)
val update_bankrupt_state : player -> bool -> player

(** [get_player_from_name lst n] returns player in list with name n *)
val get_player_from_name :
  (int * player) list -> string option -> player

(* val net_worth : player -> int *)

(* val bankrupt : player -> bool *)

(** [init_player] returns a new player initialized with default
    attributes*)
val init_player : player

(** [get_gojf pl] returns the number of "get out of jail free cards"
    player pl posseses*)
val get_gojf : player -> int

(** [add_gojf pl] returns player pl with the number of "get out of jail
    free cards" incremented by 1*)
val add_gojf : player -> player

(** [remove_gojf pl] returns player pl with the number of "get out of
    jail free cards" decremented by 1*)
val remove_gojf : player -> player

(** [incr_net_worth a pl] returns player pl with pl's networth
    incremented by amount a*)
val incr_net_worth : int -> player -> player

(** [decr_net_worth a pl] returns player pl with pl's networth
    decremented by amount a*)
val decr_net_worth : int -> player -> player
