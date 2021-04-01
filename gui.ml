open Graphics
open Board

type coord = int * int

(* replace with the name of the actual json file *)
let j = Yojson.Basic.from_file "board_monopoly.json"

let window_dim = (1280, 700)

(* must be multiple of 24 *)
let side_len = 600

let name_list = namelist (from_json j)

let price_list = pricelist (from_json j)

let color_list = colorlist (from_json j)

let getx = function a, b -> a

let gety = function a, b -> b

let botleft_coord =
  ((getx window_dim - side_len) / 2, (gety window_dim - side_len) / 2)

let botleftx = getx botleft_coord

let botlefty = gety botleft_coord

let sq_width = 2 * side_len / 24

let sq_height = 3 * side_len / 24

let botleft_coord_of_botright =
  (botleftx + side_len - sq_height, botlefty)

let botleft_coord_of_topleft =
  (botleftx, botlefty + side_len - sq_height)

let botleft_coord_of_topright =
  (botleftx + side_len - sq_height, botlefty + side_len - sq_height)

(* let c = [ ( "GO", "MEDITERRANEAN AVE", "COMMUNITY CHEST", "BALTIC
   AVE", "INCOME TAX", "READING RAILROAD" ); ] *)

let temp = [ 0; 1; 2; 3; 4; 5; 6; 7; 8 ]

let bot_coord_xlist =
  List.map (( * ) sq_width) (List.rev temp)
  |> List.map (( + ) (botleftx + sq_height))

let rec bot_coords = function
  | [] -> []
  | h :: t -> (h, botlefty) :: bot_coords t

let left_coord_ylist =
  List.map (( * ) sq_width) temp
  |> List.map (( + ) (botlefty + sq_height))

let rec left_coords = function
  | [] -> []
  | h :: t -> (botleftx, h) :: left_coords t

let top_coord_xlist =
  List.map (( * ) sq_width) temp
  |> List.map (( + ) (botleftx + sq_height))

let rec top_coords = function
  | [] -> []
  | h :: t -> (h, botlefty + side_len - sq_height) :: top_coords t

let right_coord_ylist =
  List.map (( * ) sq_width) (List.rev temp)
  |> List.map (( + ) (botlefty + sq_height))

let rec right_coords = function
  | [] -> []
  | h :: t -> (botleftx + side_len - sq_height, h) :: right_coords t

let botcoords_list =
  (botleftx + side_len - sq_height, botlefty)
  :: bot_coords bot_coord_xlist

let leftcoords_list = botleft_coord :: left_coords left_coord_ylist

let topcoords_list =
  (botleftx, botlefty + side_len - sq_height)
  :: top_coords top_coord_xlist

let rightcoords_list =
  (botleftx + side_len - sq_height, botlefty + side_len - sq_height)
  :: right_coords right_coord_ylist

let coords_list =
  botcoords_list @ leftcoords_list @ topcoords_list @ rightcoords_list

let print_tuple = function
  | x, y -> "(" ^ string_of_int x ^ "," ^ string_of_int y ^ ")"

let rec print_list = function
  | [] -> ()
  | h :: t ->
      print_string (print_tuple h);
      print_string ", ";
      print_list t

let temp = gety botleft_coord_of_topleft

let temp1 = getx botleft_coord_of_topright

let rec draw_rects = function
  | (a, b) :: t when (a, b) = botleft_coord_of_botright ->
      draw_rect a b sq_height sq_height;
      draw_rects t
  | (a, b) :: t when (a, b) = botleft_coord ->
      draw_rect a b sq_height sq_height;
      draw_rects t
  | (a, b) :: t when (a, b) = botleft_coord_of_topleft ->
      draw_rect a b sq_height sq_height;
      draw_rects t
  | (a, b) :: t when (a, b) = botleft_coord_of_topright ->
      draw_rect a b sq_height sq_height;
      draw_rects t
  | (a, b) :: t when b = botlefty ->
      draw_rect a b sq_width sq_height;
      draw_rects t
  | (a, b) :: t when a = botleftx ->
      draw_rect a b sq_height sq_width;
      draw_rects t
  | (a, b) :: t when b = temp ->
      draw_rect a b sq_width sq_height;
      draw_rects t
  | (a, b) :: t when a = temp1 ->
      draw_rect a b sq_height sq_width;
      draw_rects t
  | [] -> ()
  | h :: t -> ()

let top = ref false

let decide_top_or_bottom (a, b) str_list_length =
  if !top then
    (a + 2, b + sq_height + ((10 * (str_list_length - 1)) + 1))
    (*sq_height + (18 * str_list_length*)
  else (a + 2, b - 15)

let first_val_tuple (a, b) = match (a, b) with q, _ -> a

let second_val_tuple (a, b) = match (a, b) with _, q -> q

let rec draw_and_move str_list (a, b) =
  match str_list with
  | [] -> ()
  | h :: t ->
      moveto a b;
      draw_string h;
      moveto a (b - 10);
      draw_and_move t (a, b - 10)

let draw_names coord name =
  set_color black;
  match coord with
  | a, b when (a, b) = botleft_coord_of_botright ->
      moveto (a + (sq_height / 10)) (b + (3 * sq_height / 5));
      draw_and_move
        (String.split_on_char ' ' name)
        (a + (sq_height / 10), b + (3 * sq_height / 5))
  | a, b when (a, b) = botleft_coord ->
      moveto (a + (sq_height / 10)) (b + (3 * sq_height / 5));
      draw_and_move
        (String.split_on_char ' ' name)
        (a + (sq_height / 10), b + (3 * sq_height / 5))
  | a, b when (a, b) = botleft_coord_of_topleft ->
      moveto (a + (sq_height / 10)) (b + (3 * sq_height / 5));
      draw_and_move
        (String.split_on_char ' ' name)
        (a + (sq_height / 10), b + (3 * sq_height / 5))
  | a, b when (a, b) = botleft_coord_of_topright ->
      moveto (a + (sq_height / 10)) (b + (3 * sq_height / 5));
      draw_and_move
        (String.split_on_char ' ' name)
        (a + (sq_height / 10), b + (3 * sq_height / 5))
  | a, b when b = botlefty ->
      let moved_coords =
        decide_top_or_bottom (a, botlefty)
          (List.length (String.split_on_char ' ' name))
      in
      moveto
        (first_val_tuple moved_coords)
        (second_val_tuple moved_coords);
      draw_and_move
        (String.split_on_char ' ' name)
        (first_val_tuple moved_coords, second_val_tuple moved_coords);
      top.contents <- not !top
  | a, b when a = botleftx ->
      moveto (botleftx - 30) (b + sq_width - (sq_width / 3));
      draw_and_move
        (String.split_on_char ' ' name)
        (botleftx - 75, b + sq_width - (sq_width / 3))
  | a, b when b = temp ->
      let moved_coords =
        decide_top_or_bottom (a, temp)
          (List.length (String.split_on_char ' ' name))
      in
      moveto
        (first_val_tuple moved_coords)
        (second_val_tuple moved_coords);
      draw_and_move
        (String.split_on_char ' ' name)
        (first_val_tuple moved_coords, second_val_tuple moved_coords);
      top.contents <- not !top
  | a, b when a = temp1 ->
      moveto (a + (5 * sq_width / 3)) (b + sq_width - (sq_width / 3));
      draw_and_move
        (String.split_on_char ' ' name)
        (a + (5 * sq_width / 3), b + sq_width - (sq_width / 3))
  | a, b -> ()

let draw_colors color coord =
  match color with
  | Some (r, g, b) -> (
      match coord with
      | x, y when y = botlefty ->
          set_color (rgb r g b);
          fill_rect
            (x + 1) (*changed from x+2 to just x*)
            (y + (4 * sq_height / 5))
            (sq_width - 3)
            ((sq_height / 5) - 1)
          (* changed -4 to 0*)
      | x, y when x = botleftx ->
          set_color (rgb r g b);
          fill_rect
            (x + (4 * sq_height / 5) - 1)
            (y + 2)
            ((sq_height / 5) - 1)
            (sq_width - 3)
      | x, y when y = temp ->
          set_color (rgb r g b);
          fill_rect (x + 1) (y + 2) (sq_width - 3) ((sq_height / 5) - 2)
      | x, y when x = temp1 ->
          set_color (rgb r g b);
          fill_rect (x + 1) (y + 2) ((sq_height / 5) - 2) (sq_width - 3)
      | x, y -> ())
  | None -> ()

let draw_price price coord =
  match price with
  | Some p -> (
      match coord with
      | a, b when (a, b) = botleft_coord_of_botright ->
          moveto (a + (sq_height / 10)) (b + (3 * sq_height / 5));
          draw_string (string_of_int p)
      | a, b when (a, b) = botleft_coord ->
          moveto (a + (sq_height / 10)) (b + (3 * sq_height / 5));
          draw_string (string_of_int p)
      | a, b when (a, b) = botleft_coord_of_topleft ->
          moveto (a + (sq_height / 10)) (b + (3 * sq_height / 5));
          draw_string (string_of_int p)
      | a, b when (a, b) = botleft_coord_of_topright ->
          moveto (a + (sq_height / 10)) (b + (3 * sq_height / 5));
          draw_string (string_of_int p)
      | a, b when b = botlefty ->
          moveto (a + (sq_width / 5)) (b + (sq_height / 2));
          draw_string ("$" ^ string_of_int p)
      | a, b when a = botleftx ->
          moveto (a + (sq_height / 3)) (b + (sq_width / 2));
          draw_string ("$" ^ string_of_int p)
      | a, b when b = temp ->
          moveto (a + (sq_width / 5)) (b + (sq_height / 2));
          draw_string ("$" ^ string_of_int p)
      | a, b when a = temp1 ->
          moveto (a + (sq_height / 3)) (b + (sq_width / 2));
          draw_string ("$" ^ string_of_int p)
      | a, b -> ())
  | None -> ()

(* | (a, b) when (a, b) = botleft_coord_of_botright -> | (a, b) when (a,
   b) = botleft_coord -> | (a, b) when (a, b) = botleft_coord_of_topleft
   -> | (a, b) when (a, b) = botleft_coord_of_topright -> | (a, b) when
   b = botlefty -> | (a, b) when a = botleftx -> | (a, b) when b = temp
   -> | (a, b) when a = temp1 -> *)

let draw_horizontal_rect x y = draw_rect x y sq_width sq_height

let draw_vertical_rect x y = draw_rect y x sq_height sq_width

let coord_array = ref [||]

let append_array arr (a, b) =
  arr.contents <- Array.append !arr [| (a, b) |]

let rec draw_sqlist f y lst =
  match lst with
  | [] -> ()
  | h :: t ->
      f h y;
      append_array coord_array (h, y);
      draw_sqlist f y t

let draw_background =
  open_graph " 1280x700+100-100";
  set_window_title "Monopoly";
  set_line_width 2;
  draw_rects coords_list;
  List.iter2 draw_colors color_list coords_list;
  List.iter2 draw_names coords_list name_list;
  List.iter2 draw_price price_list coords_list

(*let move_index player dr = List.nth coords_list (Player.position
  player + dr)*)

gui_test
(*let draw_move = failwith "Unimplemented"*)

(* draw_rect botleftx botlefty side_len side_len; draw_sqlist
   draw_horizontal_rect botlefty horizontal_coord_list; draw_sqlist
   draw_horizontal_rect (botlefty + side_len - sq_height)
   horizontal_coord_list; draw_sqlist draw_vertical_rect botleftx
   vertical_coord_list; draw_sqlist draw_vertical_rect (botleftx +
   side_len - sq_height) vertical_coord_list; *)
