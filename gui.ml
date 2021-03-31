open Graphics

type coord = int * int

let window_dim = (1280, 700)

(* must be multiple of 24 *)
let side_len = 600

let getx = function a, b -> a

let gety = function a, b -> b

let botleft_coord =
  ((getx window_dim - side_len) / 2, (gety window_dim - side_len) / 2)

let botleftx = getx botleft_coord

let botlefty = gety botleft_coord

let sq_width = 2 * side_len / 24

let sq_height = 3 * side_len / 24

(* let c = [ ( "GO", "MEDITERRANEAN AVE", "COMMUNITY CHEST", "BALTIC
   AVE", "INCOME TAX", "READING RAILROAD" ); ] *)

let horizontal_coord_list =
  List.map (( * ) sq_width) [ 0; 1; 2; 3; 4; 5; 6; 7; 8 ]
  |> List.map (( + ) (botleftx + sq_height))

let vertical_coord_list =
  List.map (( * ) sq_width) [ 0; 1; 2; 3; 4; 5; 6; 7; 8 ]
  |> List.map (( + ) (botlefty + sq_height))

let draw_horizontal_rect x y = draw_rect x y sq_width sq_height

let draw_vertical_rect x y = draw_rect y x sq_height sq_width

let rec draw_sqlist f y lst =
  match lst with
  | [] -> ()
  | h :: t ->
      f h y;
      draw_sqlist f y t

let draw_background =
  open_graph " 1280x700+100-100";
  set_window_title "Monopoly";
  draw_rect botleftx botlefty side_len side_len;
  draw_sqlist draw_horizontal_rect botlefty horizontal_coord_list;
  draw_sqlist draw_horizontal_rect
    (botlefty + side_len - sq_height)
    horizontal_coord_list;
  draw_sqlist draw_vertical_rect botleftx vertical_coord_list;
  draw_sqlist draw_vertical_rect
    (botleftx + side_len - sq_height)
    vertical_coord_list
