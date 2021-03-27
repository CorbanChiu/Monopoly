type paymentstruct = (int * int) list

(** The type of values representing most square (can add houses, rent,
    etc)*)
type traditional = {
  name : string;
  pp : int;
  paymentstruct : paymentstruct;
}

(** The type of values representing properties whose rent depends on a
    dice roll*)
type utility = {
  name : string;
  pp : int;
}

(** The type of values representing properties whose rent depends on
    ownership of others*)
type railroad = {
  name : string;
  pp : int;
}

(** The type of values representing squares that give the player a card*)
type card = { name : string }

(** The type of values that do not fall into the above categories,
    namely Free Parking, Jail, Go to Jail, Go, Income Tax, Luxury Tax*)
type misc =
  | FreeParking
  | Jail
  | GoToJail
  | Go
  | IncomeTax
  | LuxuryTax

(** TODO: add documentation*)
type square =
  | Traditional
  | Utility
  | Railroad
  | Card
  | Misc

(** TODO: add documentation*)
type board = square list

let get_square b n = failwith "unimplemented"

let namelist b = failwith "unimplemented"

let pricelist b = failwith "unimplemented"

let colorlist b = failwith "unimplemented"

let developlist b = failwith "unimplemented"
