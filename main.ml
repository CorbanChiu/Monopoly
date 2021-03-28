open Graphics

(* setting everything up *)
let main () =
  open_graph " 1280x700+100-0";
  set_window_title "Monopoly";
  set_color green;
  draw_circle 640 350 100;
  ignore (read_key ())

(* Key_pressed *)

let () = main ()
