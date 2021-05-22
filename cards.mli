type cardpile

type card

type cardaction =
  | Move of int list * bool
  | Money of int list * bool
  | GOJF

val take_topcard : cardpile -> card

val from_json : string -> cardpile

val get_action : card -> cardaction
