type player = {
  pos : int;
  cash : int;
  properties : Board.square list option;
  cards : Cards.card list option;
  jail : bool;
  token : Token.token option;
  name : string option;
  bankrupt : bool;
}

let position player = player.pos

let cash player = player.cash

let properties player = player.properties

let cards player = player.cards

let jail player = player.jail

let token player = player.token

let name player = player.name

let sum_mortgage_value prop = List.fold_left ( + ) 0 prop

let int_of_square sq =
  match Board.mortgage sq with None -> 0 | Some x -> x

let intlist_of_squarelist lst = List.map int_of_square lst

(* TODO: need to include houses *)
let net_worth player =
  player.cash
  +
  match player.properties with
  | None -> 0
  | Some lst -> sum_mortgage_value (intlist_of_squarelist lst)

let bankrupt player = net_worth player >= 0
