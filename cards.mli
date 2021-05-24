(** The abstract type of value that represents a card deck/pile *)
type cardpile

(** The abstract type of value that represents a card (which serves as
    the backbone for community chest and chance cards) *)
type card

(** The abstract type of value that represents the different categories
    of cards (Move, Money, and Get out of jail free card) *)
type cardaction =
  | Move of int list * bool
  | Money of int list * bool
  | GOJF

(** [take_topcard cp] returns the top card of deck/pile cp *)
val take_topcard : cardpile -> card

(** [from_json js] returns a cardpile based on the json input js *)
val from_json : string -> cardpile

(** [get_action c] returns the cardaction (type of card) card c is *)
val get_action : card -> cardaction
