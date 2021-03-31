type cardpile

type card

type cardtype

type cardstate

val take_topcard : cardstate -> cardstate

val from_json : Yojson.Basic.t -> cardstate
