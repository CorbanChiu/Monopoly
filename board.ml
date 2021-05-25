open Yojson.Basic.Util

type paymentstruct = (int * int) list

type paymentstructure = paymentstruct option

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

type square =
  | Traditional of traditional
  | Utility of utility
  | Railroad of railroad
  | Card of card
  | Misc of misc

type board = square list

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

let get_name_from_square = function
  | Traditional sq -> sq.tname
  | Utility sq -> sq.uname
  | Railroad sq -> sq.rname
  | Card sq -> sq.cname
  | Misc sq -> get_misc_name sq

(* Returns a string list *)
let namelist b = List.map get_name_from_square b

let get_price = function
  | Traditional sq -> Some sq.tpp
  | Utility sq -> Some sq.upp
  | Railroad sq -> Some sq.rpp
  | Card sq -> None
  | Misc sq -> None

(* Returns an int option list for the purchase price *)
let pricelist b = List.map get_price b

let get_payments = function
  | Traditional sq -> (Some sq.tpaymentstruct : paymentstructure)
  | Utility sq -> Some sq.upaymentstruct
  | Railroad sq -> Some sq.rpaymentstruct
  | Card sq -> None
  | Misc sq -> None

let get_color = function Traditional sq -> Some sq.tcolor | _ -> None

(* Returns a propertycolor option list *)
let colorlist b = List.map get_color b

let get_mortgage = function
  | Traditional sq -> Some sq.tmortgageprice
  | Utility sq -> Some sq.umortgageprice
  | Railroad sq -> Some sq.rmortgageprice
  | Card sq -> None
  | Misc sq -> None

(* Returns an int option list *)
let mortgagelist b = List.map get_mortgage b

let get_buildprice = function
  | Traditional sq -> Some sq.buildingcost
  | _ -> None

let test_color b1 b2 =
  match (b1, b2) with
  | Traditional b1, Traditional b2 -> b1.tcolor = b2.tcolor
  | _ -> false

let propertygroup b sq = List.filter (test_color sq) b

let testrr = function Railroad sq -> true | _ -> false

let railroadgroup b = List.filter testrr b

let testutil = function Utility sq -> true | _ -> false

let utilitygroup b = List.filter testutil b

let get_name_from_board b sq =
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

(**************************************************)
(* End of square definition and related functions *)
(**************************************************)

let remove_option opt =
  match opt with Some a -> a | None -> failwith "no reason to call"

type property = {
  sqr : square;
  (* None indicates unbuyability while Bank indicates not owned by any
     player yet *)
  owner : string option;
  dev_lvl : int option;
  mortgage_state : bool option;
}

let get_sqr property = property.sqr

let update_sqr property s = { property with sqr = s }

let get_owner property = property.owner

let update_owner property o = { property with owner = o }

let get_dev_lvl property = property.dev_lvl

let update_dev_lvl property i = { property with dev_lvl = i }

let get_mortgage_state property = property.mortgage_state

let update_mortgage_state property b =
  { property with mortgage_state = b }

let init_property sq =
  match sq with
  | Traditional _ ->
      {
        sqr = sq;
        owner = Some "Bank";
        dev_lvl = Some 0;
        mortgage_state = Some false;
      }
  | Utility _ | Railroad _ ->
      {
        sqr = sq;
        owner = Some "Bank";
        dev_lvl = None;
        mortgage_state = Some false;
      }
  | _ ->
      { sqr = sq; owner = None; dev_lvl = None; mortgage_state = None }

let rec init_prop_lst (b : board) a =
  match b with
  | [] -> []
  | h :: t -> (a, init_property h) :: init_prop_lst t (a + 1)

let num_color_group color (b : board) =
  let color_list = colorlist b in
  if color = List.nth color_list 1 || color = List.nth color_list 39
  then 2
  else 3

let complete_propertygroup property sqr_lst b =
  List.length (propertygroup sqr_lst property.sqr)
  = num_color_group (get_color property.sqr) b

let trent_multiplier property sqr_lst b =
  if complete_propertygroup property sqr_lst b then 2 else 1

let flat_rent property =
  List.assoc
    (remove_option property.dev_lvl)
    (remove_option (get_payments property.sqr))

let trent_price property sqr_lst b =
  if property.dev_lvl = Some 0 then
    trent_multiplier property sqr_lst b * flat_rent property
  else flat_rent property

let rrent_price property sqr_lst =
  List.assoc
    (List.length (railroadgroup sqr_lst))
    (remove_option (get_payments property.sqr))

let urent_price property sqr_lst =
  List.assoc
    (List.length (utilitygroup sqr_lst))
    (remove_option (get_payments property.sqr))

let get_rent property sqr_lst b dr =
  match property.sqr with
  | Traditional sq -> trent_price property sqr_lst b
  | Utility sq -> urent_price property sqr_lst * dr
  | Railroad sq -> rrent_price property sqr_lst
  | _ -> failwith "Cannot get rent from this square"

let rec check_no_development property property_lst =
  match property_lst with
  | prop :: t ->
      if test_color property.sqr prop.sqr then
        get_dev_lvl property = Some 0 && check_no_development property t
      else check_no_development property t
  | [] -> true

let rec check_equal_helper property property_lst diff =
  match property_lst with
  | prop :: t ->
      if test_color prop.sqr property.sqr then
        (remove_option prop.dev_lvl - remove_option property.dev_lvl = 0
        || remove_option prop.dev_lvl - remove_option property.dev_lvl
           = diff)
        && check_equal_helper property t diff
      else check_equal_helper property t diff
  | [] -> true

let check_equal_development property property_lst =
  check_equal_helper property property_lst 1

let check_equal_undevelopment property property_lst =
  check_equal_helper property property_lst (-1)

let rec check_no_mortgages property property_lst =
  match property_lst with
  | prop :: t ->
      if test_color prop.sqr property.sqr then
        prop.mortgage_state = Some false
        && check_no_mortgages property t
      else check_no_mortgages property t
  | [] -> true

(****************************************************)
(* End of property definition and related functions *)
(****************************************************)
type action =
  | Buy_ok
  | Payrent_ok
  | Mortgage_ok
  | Mortgage_and_Develop_ok
  | Unmortgage_ok
  | Develop_and_Undevelop_ok
  | Undevelop_ok
  | Chance_ok
  | CC_ok
  | Freeparking_ok
  | None_ok
  | Gotojail_ok
  | Auction_ok
  | Go_ok
  | Incometax_ok
  | Luxurytax_ok

let get_action prop player_name =
  match prop.sqr with
  | Traditional _ ->
      if prop.owner = Some "Bank" then Buy_ok
      else if
        prop.owner != player_name
        && prop.owner != Some "Bank"
        && prop.mortgage_state = Some false
      then Payrent_ok
      else if
        prop.owner != Some "Bank"
        && prop.dev_lvl = Some 0
        && prop.mortgage_state = Some false
      then Mortgage_and_Develop_ok
      else if prop.mortgage_state = Some true then Unmortgage_ok
      else if
        prop.owner != Some "Bank"
        && prop.mortgage_state = Some false
        && remove_option prop.dev_lvl < 5
      then Develop_and_Undevelop_ok
      else if remove_option prop.dev_lvl = 5 then Undevelop_ok
      else None_ok
  | Utility _ | Railroad _ ->
      if prop.owner = Some "Bank" then Buy_ok
      else if
        prop.owner != player_name
        && prop.owner != Some "Bank"
        && prop.mortgage_state = Some false
      then Payrent_ok
      else if
        prop.owner != Some "Bank" && prop.mortgage_state = Some false
      then Mortgage_ok
      else if prop.mortgage_state = Some true then Unmortgage_ok
      else None_ok
  | Card c -> (
      match c.cname with
      | "Chance" -> Chance_ok
      | "Community Chest" -> CC_ok
      | _ -> failwith "this isn't the name of a real card")
  | Misc m -> (
      match m with
      | FreeParking _ -> Freeparking_ok
      | Jail _ -> None_ok
      | GoToJail _ -> Gotojail_ok
      | Go _ -> Go_ok
      | IncomeTax _ -> Incometax_ok
      | LuxuryTax _ -> Luxurytax_ok)
