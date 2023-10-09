open WordLists
open Word
open Game

let choose_category c =
  let categories =
    [
      ("HARRY POTTER", get_word_list harry_potter);
      ("CORNELL", get_word_list cornell);
      ("ZOO ANIMALS", get_word_list zoo_animals);
      ("CRAYON COLORS", get_word_list crayon_colors);
      ("DISNEY CHARACTERS", get_word_list disney_characters);
      ("COUNTRIES", get_word_list countries);
    ]
  in
  List.assoc_opt c categories

let despace str =
  let rec helper str acc =
    match str with
    | "" ->
        acc |> List.rev |> String.concat "" |> String.uppercase_ascii
    | _ ->
        let len = String.length str - 1 in
        let sub = String.sub str 1 len in
        if str.[0] = ' ' then helper sub acc
        else helper sub (Char.escaped str.[0] :: acc)
  in
  helper str []

let check_answer word guess_str = despace guess_str = despace word

(* [more_than_one lst c] returns true if there is more than one instance
   of [c] in [lst] and false otherwise *)
let more_than_one lst c =
  let find = List.filter (fun x -> x = c) lst in
  if List.length find > 1 then true else false

let print_guessed state =
  let acc = "" in
  let char_list = get_letters state in
  let rec print lst acc =
    match lst with
    | [] -> acc
    | h :: t ->
        if more_than_one lst h then print t acc
        else print t acc ^ Char.escaped h ^ " | "
  in
  print char_list acc

(* [you_win] prints out "You win!" in green. *)
let you_win () =
  ANSITerminal.print_string [ ANSITerminal.green ] "\n\nYou win!\n\n";
  exit 0

(* [no_more_guesses] prints out "You ran out of guesses. You lose :(" in
   red. *)
let no_more_guess () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nYou ran out of guesses. You lose :(\n\n"

(* [guess_prompt state] prompts the player to enter a guess and prints
   out their guesses. *)
let guess_prompt state =
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    ("\n\
      Please enter your guess here.\n\
      Enter !!! to guess the whole word. \n\
      Letters guessed: [ " ^ print_guessed state ^ "] \n\n")

(* [invalid_guess] prints out "Invalid guess!" in red. *)
let invalid_guess () =
  ANSITerminal.print_string [ ANSITerminal.red ] "\nInvalid guess!\n"

(* [wrong_guess] prints out "Wrong!" in red. *)
let wrong_guess () =
  ANSITerminal.print_string [ ANSITerminal.red ] "\nWrong!\n"

(* [print_updated_word char state word] prints the updated blanks for
   [word] with the given [char]. *)
let print_updated_word char state word =
  print_endline
    ("\n" ^ (update_blanks char state word |> Bytes.to_string) ^ "\n")

let guess_word_helper str =
  print_string "What's the word? \n > ";
  let whole_guess = read_line () |> String.uppercase_ascii in
  if whole_guess = str then "\n\nYou win!\n\n"
  else "\n\nThat wasn't the word :/\n\n"

let rec make_guess word state =
  let game_end = check_answer word (get_blanks state) in
  if game_end then you_win ()
  else if get_guesses state = 0 then (
    no_more_guess ();
    exit 0)
  else guess_prompt state;
  print_string "> ";
  let guess = read_line () |> String.uppercase_ascii in
  if guess = "!!!" then guess_whole_word word
  else if String.length guess = 1 then (
    let char = guess.[0] in
    try print_updated_word char state word with
    | InvalidCharacter x ->
        invalid_guess ();
        print_endline (get_blanks state)
    | IncorrectGuess x ->
        wrong_guess ();
        print_endline (get_blanks state))
  else invalid_guess ();
  make_guess word state

(* [guess_whole_word] is a helper function that lets the user guess the
   whole word*)
and guess_whole_word word =
  let response = guess_word_helper word in
  if response = "\n\nYou win!\n\n" then you_win ()
  else ANSITerminal.print_string [ ANSITerminal.red ] response

let play_game cat_name category =
  let word = category |> choose_word |> get_word_info in
  let blanks = word |> display_blanks in
  print_endline
    ("\nYou have chosen a word in the " ^ cat_name ^ " category.\n");
  print_endline blanks;
  let init_state = word |> init_state in
  let word_str = get_word word in
  make_guess word_str init_state
