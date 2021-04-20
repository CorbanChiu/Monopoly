open Yojson.Basic.Util

type paymentstruct = (int * int) list

type propertycolor = int * int * int

exception UnknownJSON

(* The type of values representing most square (can add houses, rent,
   etc)*)
type traditional = {
  tname : string;
  tpp : int;
  tpaymentstruct : paymentstruct;
  tcolor : propertycolor;
  tmortgageprice : int;
  buildingcost : int;
}

(* The type of values representing properties whose rent depends on a
   dice roll*)
type utility = {
  uname : string;
  upp : int;
  upaymentstruct : paymentstruct;
  umortgageprice : int;
}

(* The type of values representing properties whose rent depends on
   ownership of others*)
type railroad = {
  rname : string;
  rpp : int;
  rpaymentstruct : paymentstruct;
  rmortgageprice : int;
}

(* The type of values representing squares that give the player a card*)
type card = {
  cname : string;
  ctype : string;
  cnum : int;
}

type freeparking = { fpname : string }

type jail = { jname : string }

type gotojail = { gtjname : string }

type go = { gname : string }

type incometax = { itname : string }

type luxurytax = { ltname : string }

(* The type of values that do not fall into the above categories, namely
   Free Parking, Jail, Go to Jail, Go, Income Tax, Luxury Tax*)
type misc =
  | FreeParking of freeparking
  | Jail of jail
  | GoToJail of gotojail
  | Go of go
  | IncomeTax of incometax
  | LuxuryTax of luxurytax

(**TODO: add documentation*)
type square =
  | Traditional of traditional
  | Utility of utility
  | Railroad of railroad
  | Card of card
  | Misc of misc

(* TODO: add documentation*)
type board = square list

(* TODO: bad *)
let rec to_paymentstruct = function
  | (a, b) :: t ->
      (a |> int_of_string, b |> to_int) :: to_paymentstruct t
  | [] -> []

let tuple_of_list lst =
  let _ = assert (List.length lst = 3) in
  (List.nth lst 0, List.nth lst 1, List.nth lst 2)

let to_color j = j |> to_list |> List.map to_int |> tuple_of_list

let to_traditional j =
  Traditional
    {
      tname = j |> member "name" |> to_string;
      tpp = j |> member "purchase price" |> to_int;
      tpaymentstruct =
        j |> member "payment structure" |> to_assoc |> to_paymentstruct;
      tcolor = j |> member "color" |> to_color;
      tmortgageprice = j |> member "mortgage price" |> to_int;
      buildingcost = j |> member "building cost" |> to_int;
    }

let to_utility j =
  Utility
    {
      uname = j |> member "name" |> to_string;
      upp = j |> member "purchase price" |> to_int;
      upaymentstruct =
        j |> member "payment structure" |> to_assoc |> to_paymentstruct;
      umortgageprice = j |> member "mortgage price" |> to_int;
    }

let to_railroad j =
  Railroad
    {
      rname = j |> member "name" |> to_string;
      rpp = j |> member "purchase price" |> to_int;
      rpaymentstruct =
        j |> member "payment structure" |> to_assoc |> to_paymentstruct;
      rmortgageprice = j |> member "mortgage price" |> to_int;
    }

let to_card j =
  Card
    {
      cname = j |> member "name" |> to_string;
      ctype = j |> member "card type" |> to_string;
      cnum = j |> member "order" |> to_int;
    }

let to_misc j =
  j |> member "name" |> to_string |> function
  | "Free Parking" -> Misc (FreeParking { fpname = "Free Parking" })
  | "Jail" -> Misc (Jail { jname = "Jail" })
  | "Go To Jail" -> Misc (GoToJail { gtjname = "Go To Jail" })
  | "Go" -> Misc (Go { gname = "Go" })
  | "Income Tax" -> Misc (IncomeTax { itname = "Income Tax" })
  | "Luxury Tax" -> Misc (LuxuryTax { ltname = "Luxury Tax" })
  | _ -> raise UnknownJSON

let to_square j =
  j |> member "type" |> to_string |> function
  | "Traditional" -> to_traditional (j |> member "square")
  | "Utility" -> to_utility (j |> member "square")
  | "Railroad" -> to_railroad (j |> member "square")
  | "Card" -> to_card (j |> member "square")
  | "Miscellaneous" -> to_misc (j |> member "square")
  | _ -> raise UnknownJSON

let from_json j = j |> to_list |> List.map to_square

let get_square b n = List.nth b n

let square_equal sq1 sq2 = sq1 = sq2

let find_square (b : board) sq =
  let rec index_rec i = function
    | [] -> failwith "doesn't exist"
    | h :: t -> if h = sq then i else index_rec (i + 1) t
  in
  index_rec 0 b

let get_misc_name = function
  | FreeParking m -> m.fpname
  | Jail m -> m.jname
  | GoToJail m -> m.gtjname
  | Go m -> m.gname
  | IncomeTax m -> m.itname
  | LuxuryTax m -> m.ltname

let get_name = function
  | Traditional sq -> sq.tname
  | Utility sq -> sq.uname
  | Railroad sq -> sq.rname
  | Card sq -> sq.cname
  | Misc sq -> get_misc_name sq

(* Returns a string list *)
let namelist b = List.map get_name b

let get_price = function
  | Traditional sq -> Some sq.tpp
  | Utility sq -> Some sq.upp
  | Railroad sq -> Some sq.rpp
  | Card sq -> None
  | Misc sq -> None

(* Returns an int option list for the purchase price *)
let pricelist b = List.map get_price b

let get_paymentstruct = function
  | Traditional sq -> Some sq.tpaymentstruct
  | Utility sq -> Some sq.upaymentstruct
  | Railroad sq -> Some sq.rpaymentstruct
  | Card sq -> None
  | Misc sq -> None

let get_color = function Traditional sq -> Some sq.tcolor | _ -> None

(* Returns a propertycolor option list *)
let colorlist b = List.map get_color b

let get_mortage = function
  | Traditional sq -> Some sq.tmortgageprice
  | Utility sq -> Some sq.umortgageprice
  | Railroad sq -> Some sq.rmortgageprice
  | Card sq -> None
  | Misc sq -> None

(* Returns an int option list *)
let mortgagelist b = List.map get_mortage b

let mortgage = get_mortage

let test_color b1 b2 =
  match (b1, b2) with
  | Traditional b1, Traditional b2 -> b1.tcolor = b2.tcolor
  | _ -> false

let propertygroup b sq = List.filter (test_color sq) b

let testrr = function Railroad sq -> true | _ -> false

let railroadgroup b = List.filter testrr b

let testutil = function Utility sq -> true | _ -> false

let utilitygroup b = List.filter testutil b

let get_name b sq =
  List.find (( = ) sq) b |> function
  | Traditional { tname } -> tname
  | Utility { uname } -> uname
  | Railroad { rname } -> rname
  | Card { cname } -> cname
  | Misc m -> (
      match m with
      | FreeParking { fpname } -> fpname
      | Jail { jname } -> jname
      | GoToJail { gtjname } -> gtjname
      | Go { gname } -> gname
      | IncomeTax { itname } -> itname
      | LuxuryTax { ltname } -> ltname)

type property = {
  sqr : square;
  (* None indicates unbuyability while Bank indicates not owned by any
     player yet *)
  owner : string option;
  dev_lvl : int option;
  mortgaged : bool option;
}

type action =
  | Buy_ok
  | Auction_ok
  | Payrent_ok
  | Mortgage_ok
  | Card_ok
  | Freeparking_ok
  | None
  | Gotojail_ok
  | Go_ok
  | Incometax_ok
  | Luxurytax_ok

let get_action prop player =
  match prop.sqr with
  | Traditional _ | Utility _ | Railroad _ ->
      if prop.owner = Some "Bank" then Buy_ok
      else if prop.owner = Some player then Mortgage_ok
      else Payrent_ok
  | Card _ -> Card_ok
  | Misc m -> (
      match m with
      | FreeParking _ -> Freeparking_ok
      | Jail _ -> None
      | GoToJail _ -> Gotojail_ok
      | Go _ -> Go_ok
      | IncomeTax _ -> Incometax_ok
      | LuxuryTax _ -> Luxurytax_ok)

let get_sqr prop = prop.sqr

let get_owner prop = prop.owner

let get_dev_lvl prop = prop.dev_lvl

let get_mortgaged prop = prop.mortgaged

let init_property sq =
  match sq with
  | Traditional _ ->
      {
        sqr = sq;
        owner = Some "Bank";
        dev_lvl = Some 0;
        mortgaged = Some false;
      }
  | Utility _ ->
      {
        sqr = sq;
        owner = Some "Bank";
        dev_lvl = None;
        mortgaged = Some false;
      }
  | Railroad _ ->
      {
        sqr = sq;
        owner = Some "Bank";
        dev_lvl = None;
        mortgaged = Some false;
      }
  | _ -> { sqr = sq; owner = None; dev_lvl = None; mortgaged = None }

let rec init_prop_lst (b : board) a =
  match b with
  | [] -> []
  | h :: t -> (a, init_property h) :: init_prop_lst t (a + 1)

let update_property_new_owner prop owner_name =
  { prop with owner = owner_name }

let get_property_square (prop : property) = prop.sqr

(* let same_color color sqr = match sqr with | Traditional _ -> color =
   sqr.tcolor | _ -> false *)

let rec num_same_color color sqr_lst =
  match color with
  | r, g, b -> (
      match sqr_lst with
      | [] -> 0
      | h :: t -> (
          match get_color h with
          | Some (r1, g1, b1) when (r1, g1, b1) = (r, g, b) ->
              1 + num_same_color color t
          | _ -> num_same_color color t))

let rec num_railroads sqr_lst =
  match sqr_lst with
  | [] -> 0
  | h :: t -> (
      match h with
      | Railroad _ -> 1 + num_railroads t
      | _ -> num_railroads t)

let rec num_utilities sqr_lst =
  match sqr_lst with
  | [] -> 0
  | h :: t -> (
      match h with
      | Utility _ -> 1 + num_utilities t
      | _ -> num_utilities t)

(* [same_group sq lst] returns the number of squares in square list lst
   that have the same type as square sq or the rent multiplier if the
   square has type traditional. *)
let same_group sqr sqr_lst =
  match sqr with
  | Railroad _ -> num_railroads sqr_lst
  | Utility _ -> num_utilities sqr_lst
  | _ -> 1

let get_rent sqr ind mult dr =
  match sqr with
  | Traditional
      {
        tname;
        tpp;
        tpaymentstruct;
        tcolor;
        tmortgageprice;
        buildingcost;
      } ->
      List.assoc ind tpaymentstruct * mult
  | Utility { uname; upp; upaymentstruct; umortgageprice } ->
      List.assoc ind upaymentstruct * dr
  | Railroad { rname; rpp; rpaymentstruct; rmortgageprice } ->
      List.assoc ind rpaymentstruct
  | _ -> failwith "Cannot get rent from this square"
