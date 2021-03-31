open Graphics
open Gui

(* setting everything up *)

(* let ourdraw_rect x = draw_rect x 50 50 75 *)

(* let ft : unit list = List.map ourdraw_rect coord_list *)

let main () =
  draw_background;
  ignore (read_key ())

(* Key_pressed *)

let () = main ()
