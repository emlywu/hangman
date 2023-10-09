open Gui
open Graphics
open Terminal
open Score
open Menu

let terminal () =
  print_endline
    "\n\
     Please enter the name of the category you'd like to play: HARRY \
     POTTER, CORNELL, ZOO ANIMALS, CRAYON COLORS, DISNEY CHARACTERS,  \
     or COUNTRIES.";
  print_string "> ";
  let cat_name = read_line () |> String.uppercase_ascii in
  match choose_category cat_name with
  | Some c -> play_game cat_name c
  | None -> print_endline "\nThis is not a valid category."

let rec gui round_1 score_opt p =
  open_graph " 1000x700";
  set_window_title "Hangman";
  let width = size_x () in
  let height = size_y () in
  if round_1 then
    try start_game width height None p with
    | Graphic_failure x -> close_graph ()
    | Start_Over (s, p) -> gui false (Some s) (new_round p)
  else
    try start_game width height score_opt p with
    | Graphic_failure x -> close_graph ()
    | Start_Over (s, p) -> gui false (Some s) (new_round p)

let main () =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\nWelcome to Hangman!\n";
  print_endline
    "Please enter T if you'd like to play in the terminal or G if \
     you'd like to play with graphics.";
  print_string "> ";
  let choice = read_line () |> String.uppercase_ascii in
  if choice = "T" then terminal () else gui true None init_settings

let () = main ()
