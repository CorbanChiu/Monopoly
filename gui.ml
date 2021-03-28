open Graphics

type coord = int * int

(* let c = [ ( "GO", "MEDITERRANEAN AVE", "COMMUNITY CHEST", "BALTIC
   AVE", "INCOME TAX", "READING RAILROAD" ); ] *)

let horizontal_coord_list =
  [ 815; 765; 715; 665; 615; 565; 515; 465; 415 ]

let vertical_coord_list =
  [ 125; 175; 225; 275; 325; 375; 425; 475; 525 ]

let draw_horizontal_rect x y = draw_rect x y 50 75

let draw_vertical_rect x y = draw_rect y x 75 50

let rec draw_sqlist f y lst =
  match lst with
  | [] -> ()
  | h :: t ->
      f h y;
      draw_sqlist f y t

let draw_board =
  open_graph " 1280x700+100-100";
  set_window_title "Monopoly";
  draw_rect 340 50 600 600;
  draw_sqlist draw_horizontal_rect 50 horizontal_coord_list;
  draw_sqlist draw_horizontal_rect 575 horizontal_coord_list;
  draw_sqlist draw_vertical_rect 340 vertical_coord_list;
  draw_sqlist draw_vertical_rect 865 vertical_coord_list
