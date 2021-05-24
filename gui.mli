(** Responsible for launching the GUI that displays the Monopoly
    interface *)

(** [play_game js] creates the monopoly game displayed within the users
    window using initialization information from json string list js *)
val play_game : string list -> unit
