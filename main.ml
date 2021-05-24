let ask_names () =
  for i = 1 to 50 do
    print_endline ""
  done;
  let names = ref [] in
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "Let's play Monopoly! This is a 4-person game developed by Sunny, \
     Corban, and Connor. Please enter four unique player names.\n";
  print_endline "Please enter the name of the first player:";
  print_string "> ";
  (match read_line () with
  | exception End_of_file -> ()
  | name -> names := name :: !names);
  print_endline "Please enter the name of the second player:";
  print_string "> ";
  (match read_line () with
  | exception End_of_file -> ()
  | name -> names := name :: !names);
  print_endline "Please enter the name of the third player:";
  print_string "> ";
  (match read_line () with
  | exception End_of_file -> ()
  | name -> names := name :: !names);
  print_endline "Please enter the name of the fourth player:";
  print_string "> ";
  (match read_line () with
  | exception End_of_file -> ()
  | name -> names := name :: !names);
  names := List.rev !names;
  ANSITerminal.print_string [ ANSITerminal.blue ] "Have fun! \n";
  !names

let () =
  let winner = Gui.play_game (ask_names ()) in
  for i = 1 to 50 do
    print_endline ""
  done;
  ANSITerminal.print_string [ ANSITerminal.blue ]
    ("Congratulations to " ^ winner ^ "! You won!");
  print_endline ""
