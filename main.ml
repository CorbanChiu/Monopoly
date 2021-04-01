open Graphics
open Gui
open Player

let main () =
  draw_background;
  draw_state State.init;
  ignore (read_key ())

(* Key_pressed *)

let () = main ()
