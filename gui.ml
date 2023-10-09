open Graphics
open Game
open Word
open WordLists
open Score
open Menu
open Draw

type settings = {
  show_keys : bool;
  palette : palette;
}

let init_settings = { show_keys = false; palette = standard_palette }

let new_round s = { s with show_keys = false }

exception Start_Over of score * settings

(* [lt_green], [dk_green], and [md_green] are the default colors for
   buttons in the standard palette. *)
let lt_green = rgb 129 194 118

let dk_green = rgb 83 138 74

let md_green = rgb 106 168 96

(* [clear p] clears the window and fills it with the background color. *)
let clear p =
  clear_graph ();
  set_color (background_color p);
  fill_rect 0 0 1000 700

(*********************** DISPLAY SCORE ******************************** *)
let draw_score s p =
  set_color (text_color p);
  moveto 30 650;
  draw_string "SCORE: ";
  moveto 70 650;
  let get_score = get_current_score s |> Int.to_string in
  draw_string get_score

let draw_round_score s p =
  set_color (text_color p);
  moveto 480 350;
  draw_string "ROUND SCORE: ";
  moveto 555 350;
  let round_score = get_current_score s |> Int.to_string in
  draw_string round_score

(************************* DISPLAY GUESSES ***************************** *)

let init_guesses_box x y w h p =
  set_color (drawing_color p);
  draw_rect x y w h;
  moveto (x + 10) (y + 100);
  set_color (text_color p);
  draw_string "Letters Guessed: "

let draw_guesses x y c st =
  let num_guesses = List.length (get_incorrect st) in
  if current_x () = 965 then (
    moveto (x + (10 * (num_guesses - 18))) (y - 20);
    draw_char c)
  else (
    moveto (x + (10 * num_guesses)) y;
    draw_char c)

let new_guess c st =
  let guess_list = get_letters st in
  List.mem c guess_list

(* [despace str] removes the spaces in [str]. *)
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

let game_over st word =
  check_answer word (get_blanks st) || get_guesses st = 0

(************************* DISPLAY WIN/LOSS **************************** *)

(* [display_win st word w score p] draws a banner, letting the user know
   that they have won the current round. *)
let display_win st word w score p =
  clear p;
  moveto ((w / 2) - 50) 375;
  Draw.draw_banner_win 450 368 144 26 (button_med_color p);
  set_color (text_color p);
  draw_string "Congratulations! You win!";
  draw_round_score score p

(* [display_lose st word w score p] draws a banner, letting the user
   know that they have lost the current round. *)
let display_lose st word w score p =
  clear p;
  moveto ((w / 2) - 45) 375;
  Draw.draw_banner_lose 450 368 144 26 (button_med_color p);
  set_color (text_color p);
  draw_string "You ran out of guesses!";
  draw_round_score score p

let display_win_lose st word w score p =
  moveto 300 300;
  let win = check_answer word (get_blanks st) in
  if win then display_win st word w score p
  else display_lose st word w score p

(**************************** DRAWING HANGMAN ************************** *)

type status =
  | ShouldDraw
  | ShouldNotDraw
  | Drawn

type body_record = {
  difficulty : bool;
  shift : int;
  hat : status;
  head : status;
  body : status;
  l_arm : status;
  r_arm : status;
  l_leg : status;
  r_leg : status;
  l_foot : status;
  r_foot : status;
  dead_face : status;
}

let hard_body r =
  {
    difficulty = true;
    shift = int_of_float (2.0 *. float_of_int r *. 0.33333333);
    hat = ShouldDraw;
    head = ShouldDraw;
    body = ShouldDraw;
    l_arm = ShouldDraw;
    r_arm = ShouldDraw;
    l_leg = ShouldDraw;
    r_leg = ShouldDraw;
    l_foot = ShouldDraw;
    r_foot = ShouldDraw;
    dead_face = ShouldDraw;
  }

let easy_body =
  {
    difficulty = false;
    shift = 0;
    hat = ShouldNotDraw;
    head = ShouldDraw;
    body = ShouldDraw;
    l_arm = ShouldDraw;
    r_arm = ShouldDraw;
    l_leg = ShouldDraw;
    r_leg = ShouldDraw;
    l_foot = ShouldNotDraw;
    r_foot = ShouldNotDraw;
    dead_face = ShouldNotDraw;
  }

let instantiate_body difficulty r =
  if difficulty = true then hard_body r else easy_body

(* [draw_dead_face r] draws the eyes and frown for the dead face. *)
let draw_dead_face r =
  draw_left_eye 600 (475 - r.shift) 30;
  draw_right_eye 600 (475 - r.shift) 30;
  draw_dead_smile 600 (475 - r.shift) 30

(* [draw_foot r f s] draws the foot specified by [f] and [s] (either
   left or right) based off of body_record [r]. *)
let draw_foot r f s =
  let x0, y0 = find_point 600 (345 - r.shift) 50 s in
  f x0 y0 30

let draw_next r p =
  set_color (drawing_color p);
  if r.hat = ShouldDraw then (
    draw_hat 600 (505 - r.shift) 30;
    { r with hat = Drawn })
  else if r.head = ShouldDraw then (
    draw_head 600 (475 - r.shift) 30;
    { r with head = Drawn })
  else if r.body = ShouldDraw then (
    draw_body 600 (345 - r.shift) 100;
    { r with body = Drawn })
  else if r.l_arm = ShouldDraw then (
    draw_l_limb 600 (420 - r.shift) 50;
    { r with l_arm = Drawn })
  else if r.r_arm = ShouldDraw then (
    draw_r_limb 600 (420 - r.shift) 50;
    { r with r_arm = Drawn })
  else if r.l_leg = ShouldDraw then (
    draw_l_limb 600 (345 - r.shift) 50;
    { r with l_leg = Drawn })
  else if r.r_leg = ShouldDraw then (
    draw_r_limb 600 (345 - r.shift) 50;
    { r with r_leg = Drawn })
  else if r.l_foot = ShouldDraw then (
    draw_foot r draw_foot_l "left";
    { r with l_foot = Drawn })
  else if r.r_foot = ShouldDraw then (
    draw_foot r draw_foot_r "right";
    { r with r_foot = Drawn })
  else if r.dead_face = ShouldDraw then (
    draw_dead_face r;
    { r with dead_face = Drawn })
  else r

(************************* CHOOSE CATEGORIES *************************** *)

let choose_category c =
  let categories =
    [
      ("HARRY POTTER", get_word_list harry_potter);
      ("CORNELL", get_word_list cornell);
      ("ZOO ANIMALS", get_word_list zoo_animals);
      ("CRAYON COLORS", get_word_list crayon_colors);
      ("DISNEY CHARACTERS", get_word_list disney_characters);
      ("COUNTRIES", get_word_list countries);
      ("OCAML", get_word_list ocaml);
    ]
  in
  List.assoc_opt c categories

(* [extract opt] is the array associated with an array option [opt], or
   the empty array if [opt] is None. *)
let extract opt = match opt with Some x -> x | None -> [||]

(* [choose_cat] is the name of the category chosen by the user's mouse
   click. *)
let rec choose_cat () =
  if mouse_in_button 60 300 120 75 then "CORNELL"
  else if mouse_in_button 210 300 120 75 then "DISNEY CHARACTERS"
  else if mouse_in_button 360 300 120 75 then "CRAYON COLORS"
  else if mouse_in_button 510 300 120 75 then "ZOO ANIMALS"
  else if mouse_in_button 660 300 120 75 then "HARRY POTTER"
  else if mouse_in_button 810 300 120 75 then "COUNTRIES"
  else if mouse_in_button 530 602 30 30 then "OCAML"
  else if mouse_in_button 25 625 60 35 then "MENU"
  else (
    ignore (wait_next_event [ Button_down ]);
    choose_cat ())

(* [draw_cats] draws the button for each category and for the menu. *)
let draw_cats palette =
  let lt_button = button_light_color palette in
  let md_button = button_med_color palette in
  let dk_button = button_dark_color palette in
  draw_button (rgb 232 232 232) (rgb 194 194 194) (rgb 150 150 150) 25
    625 60 35 5 8 "MENU";
  draw_button lt_button md_button dk_button 60 300 120 75 5 8 "Cornell";
  draw_button lt_button md_button dk_button 210 300 120 75 5 8
    "Disney Characters";
  draw_button lt_button md_button dk_button 360 300 120 75 5 8
    "Crayon Colors";
  draw_button lt_button md_button dk_button 510 300 120 75 5 8
    "Zoo Animals";
  draw_button lt_button md_button dk_button 660 300 120 75 5 8
    "Harry Potter";
  draw_button lt_button md_button dk_button 810 300 120 75 5 8
    "Countries"

(************************* MENU *************************** *)

let rec choose_menu_option () =
  ignore (wait_next_event [ Button_down ]);
  if mouse_in_button 450 175 100 45 then "instructions"
  else if mouse_in_button 450 280 100 45 then "palette"
  else if mouse_in_button 450 385 100 45 then "leaderboard"
  else if mouse_in_button 450 490 100 45 then "keyboard"
  else if mouse_in_button 210 515 50 50 then "exit"
    (* clear_graph (); draw_cats (); choose_cat ()) *)
  else (
    ignore (wait_next_event [ Button_down ]);
    choose_menu_option ())

(*********************** DISPLAY HINTS ***************************** *)

(* helper function for [make_guess]: [draw_hint] will display a hint
   button. *)
let draw_hint p =
  draw_button (button_light_color p) (button_med_color p)
    (button_dark_color p) 810 300 120 75 5 8 "PRESS ? FOR HINT"

(* helper function for [make_guess]: [need_hint] will display hint
   button if current state of game detects that a hint may be needed. *)
let need_hint word st p =
  let uniqs = Word.unique_letters word in
  let difficulty_word = if uniqs <= 7 then 6 else 10 in
  let difficult = if difficulty_word = 6 then false else true in
  let num_guesses_made = List.length (get_incorrect st) in
  if difficult = false && num_guesses_made = 4 then draw_hint p
  else if difficult = true && num_guesses_made = 7 then draw_hint p

let make_char_list str =
  let new_str = despace str in
  let rec helper n lst =
    if n < 0 then lst else helper (n - 1) (new_str.[n] :: lst)
  in
  helper (String.length new_str - 1) []

(*helper function for [reveal_letter]: [list_rand] will select a random
  element in a list. *)
let list_rand lst =
  let len = List.length lst in
  List.nth lst (Random.int len)

let reveal_letter word state =
  let letters_guessed = Game.get_letters state in
  let letters = make_char_list word in
  let reveal_list =
    List.filter (fun x -> List.mem x letters_guessed = false) letters
  in
  let random = list_rand reveal_list in
  random

(*************************** GAME PROGRESSION ************************* *)
let rec play_again score settings =
  let p = settings.palette in
  Graphics.set_color black;
  draw_button (button_light_color p) (button_med_color p)
    (button_dark_color p) 475 275 100 50 5 8 "Play again?";
  ignore (wait_next_event [ Button_down ]);
  if mouse_in_button 475 275 100 50 then
    raise (Start_Over (score, settings))
  else play_again score settings

(* [initialize width height word_width] is a helper function that starts
   off the screen on every make_guess call. *)
let initialize width height word_width palette =
  moveto ((width / 2) - 55) (7 * height / 8);
  set_color (text_color palette);
  set_text_size 20;
  draw_string "Please enter your guess.";
  moveto ((width / 2) - 10) (height / 8);
  set_color (background_color palette);
  draw_string "Wrong";
  moveto ((width / 2) - (word_width / 2)) (height / 6);
  set_color Graphics.black

(* [update_and_print] is a helper function that updates the blanks on
   the screen. *)
let update_and_print width word_width height guess state word =
  moveto ((width / 2) - (word_width / 2)) (height / 6);
  draw_string (update_blanks guess state word |> Bytes.to_string)

(* [draw_wrong] is a helper function that draws "wrong" on the screen in
   the bottom center. *)
let draw_wrong width height =
  set_color Graphics.red;
  moveto ((width / 2) - 10) (height / 8);
  draw_string "Wrong"

(* [update_rscore s] updates the score object [s] during a round *)
let update_rscore s hint c w p =
  if hint then update_after_hint s else update_round_score c w s;
  draw_score s p

(* [erase_old_score s p] erases the score currently on the screen in the
   upper left corner. *)
let erase_old_score s p =
  set_color (background_color p);
  moveto 30 650;
  draw_string "SCORE: ";
  moveto 70 650;
  let get_score = get_current_score s |> Int.to_string in
  draw_string get_score

(* [get_this_guess kb] detects if the keyboard function is enabled. If
   the keyboard is not enabled then the character typed by the player
   will be read as the player's guess. Otherwise, if the keyboard is
   enabled, then the character they click on the keyboard will be
   detected, and read as their guess. *)
let get_this_guess kb =
  if not kb then read_key () |> Char.uppercase_ascii
  else detect_letter 50 150

(* [game_is_over state word width score palette settings] displays
   whether the player won or loss, updates their score, and asks them if
   they'd like to play again. *)
let game_is_over state word width score palette settings =
  ignore (wait_next_event [ Key_pressed; Button_down ]);
  display_win_lose state word width score palette;
  let win = check_answer word (get_blanks state) in
  let new_score_obj =
    update_score score (get_current_score score) win |> final_score
  in
  let old_lb = pretty_score_list in
  let updated_lb = update_leaderboard old_lb new_score_obj in
  write_leaderboard updated_lb;
  play_again new_score_obj settings

let rec make_guess word state width height word_width r score settings =
  let palette = settings.palette in
  let keyboard_on = settings.show_keys in
  if keyboard_on then show_keyboard true 50 150 (text_color palette);
  if game_over state word then
    game_is_over state word width score palette settings;
  initialize width height word_width palette;
  let guess = get_this_guess keyboard_on in
  if guess = '?' then
    get_hint word state palette width word_width height score guess r
      settings
  else
    try
      try_guess score palette guess word width height word_width r
        settings state
    with
    | InvalidCharacter x ->
        invalid_char r palette score word state width height word_width
          settings
    | IncorrectGuess x ->
        incorrect_guess score palette guess word width height x state r
          word_width settings

(* [try_guess] is a helper function for make_guess. [try_guess] will
   update the player's score and allow them to guess again. *)
and try_guess
    score
    palette
    guess
    word
    width
    height
    word_width
    r
    settings
    state =
  erase_old_score score palette;
  update_rscore score false guess word palette;
  set_color (text_color palette);
  update_and_print width word_width height guess state word;
  make_guess word state width height word_width r score settings

(* [get_hint w state p width word_width height score guess r s] reveals
   a letter, updates and prints the blanks, and updates the user's score
   if they choose to receive a hint. *)
and get_hint w state p width word_width height score guess r s =
  let hint_guess = reveal_letter w state in
  set_color (text_color p);
  update_and_print width word_width height hint_guess state w;
  erase_old_score score p;
  update_rscore score true guess w p;
  make_guess w state width height word_width r score s

(* [invalid_char r palette score w state width height word_width s]
   updates the score and recalls make_guess after an invalid character
   is guessed. *)
and invalid_char r palette score w state width height word_width s =
  let new_r = draw_next r palette in
  set_color (text_color palette);
  draw_score score palette;
  make_guess w state width height word_width new_r score s

(* [incorrect_guess score palette g w width height x st r word_width s]
   updates the score and recalls make_guess after an incorrect guess is
   made. *)
and incorrect_guess score palette g w width height x st r word_width s =
  erase_old_score score palette;
  update_rscore score false g w palette;
  set_color (text_color palette);
  draw_wrong width height;
  if new_guess x st then draw_guesses 775 630 x st;
  need_hint w st palette;
  set_color (drawing_color palette);
  let new_r = draw_next r palette in
  make_guess w st width height word_width new_r score s

let play_game category w h score settings =
  let palette = settings.palette in
  let word = category |> choose_word |> get_word_info in
  let blanks = word |> display_blanks in
  let width = String.length blanks * 5 in
  clear palette;
  init_guesses_box 775 550 200 100 palette;
  set_color (drawing_color palette);
  draw_gallows 400 550 350 200 45 100 300;
  moveto ((w / 2) - (width / 2)) (h / 6);
  set_color (text_color palette);
  draw_string blanks;
  let str_w = get_word word in
  init_round_score (get_word_info str_w) score;
  draw_score score palette;
  let init_state = word |> init_state in
  let word_str = get_word word in
  let difficult = if word_difficulty word = 6 then false else true in
  make_guess word_str init_state w h width
    (instantiate_body difficult 30)
    score settings

let erase_username c1 c2 c3 width background =
  set_color background;
  fill_rect ((width / 2) - 70) 250 250 100;
  moveto ((width / 2) - 100) 425;
  draw_string "Please enter a 3-letter username.";
  draw_rect ((width / 2) - 25) 380 50 20;
  moveto ((width / 2) - 15) 384;
  draw_char c1;
  moveto ((width / 2) - 1) 384;
  draw_char c2;
  moveto ((width / 2) + 12) 384;
  draw_char c3

(* [read_username width text] reads the username inputted by the user
   and returns it. *)
let read_username width text =
  set_color background;
  fill_rect ((width / 2) - 70) 250 250 100;
  set_color text;
  let first_char = read_key () |> Char.uppercase_ascii in
  draw_char first_char;
  moveto ((width / 2) - 1) 384;
  let second_char = read_key () |> Char.uppercase_ascii in
  draw_char second_char;
  moveto ((width / 2) + 12) 384;
  let third_char = read_key () |> Char.uppercase_ascii in
  draw_char third_char;
  ignore (wait_next_event [ Button_down; Key_pressed ]);
  erase_username first_char second_char third_char width background;
  Char.escaped first_char
  ^ Char.escaped second_char
  ^ Char.escaped third_char

let get_username width background text =
  moveto ((width / 2) - 100) 425;
  draw_string "Please enter a 3-letter username.";
  draw_button lt_green md_green dk_green
    ((width / 2) - 50)
    300 110 40 5 8 "Enter as guest.";
  draw_rect ((width / 2) - 25) 380 50 20;
  moveto ((width / 2) - 15) 384;
  ignore (wait_next_event [ Button_down; Key_pressed ]);
  if mouse_in_button ((width / 2) - 50) 300 110 40 then (
    erase_username 'A' 'A' 'A' width background;
    "GST")
  else read_username width text

(* [extract_score s] returns the score object from [s]. *)
let extract_score s =
  match s with Some s' -> s' | None -> init_score "non"

(* [detect_next_action x exit_y back_y] detects the user's next action.
   [detect_next_action] will return "exit" if the exit button has been
   chosen, or "back" if the back button has been chosen*)
let rec detect_next_action x exit_y back_y =
  ignore (wait_next_event [ Button_down ]);
  if mouse_in_button x exit_y 30 30 then "exit"
  else if mouse_in_button x back_y 40 25 then "back"
  else detect_next_action x exit_y back_y

(* [make_lb lb] clears the screen and draws the leaderboard. *)
let make_lb lb =
  clear_graph ();
  exit_window_button 10 665;
  back_button 10 630;
  draw_lb_lines 1000 700 lb

(* [cat_is_menu] takes the user to the menu, allows them to select their
   choice off of the menu, and returns a settings object reflecting
   their choices. *)
let rec cat_is_menu old_palette lb =
  clear_graph ();
  show_menu ();
  let choice = choose_menu_option () in
  if choice = "palette" then (
    make_palette ();
    let new_palette = choose_palette () in
    { show_keys = false; palette = new_palette })
  else if choice = "keyboard" then
    { show_keys = true; palette = old_palette }
  else if choice = "leaderboard" then (
    make_lb lb;
    let next_action = detect_next_action 10 665 630 in
    do_next next_action false old_palette lb)
  else if choice = "instructions" then (
    make_instructions ();
    let next_action = detect_next_action 210 515 475 in
    do_next next_action false old_palette lb)
  else if choice = "back" then
    { show_keys = false; palette = old_palette }
  else { show_keys = false; palette = old_palette }

(* [do_next action key_setting old_palette lb] takes an action, either
   to exit the menu or go back in the menu, and returns the appropriate
   settings object*)
and do_next action key_setting old_palette lb =
  if action = "exit" then
    { show_keys = key_setting; palette = old_palette }
  else cat_is_menu old_palette lb

(* [choose_cat_or_menu width height score palette] asks the user for
   their cateogry choice, directs them to the menu if they click there,
   and repeats the process until a category is chosen. *)
let rec choose_cat_or_menu width height score settings lb =
  let palette = settings.palette in
  clear palette;
  draw_cats palette;
  set_color (drawing_color palette);
  Draw.draw_secret_man 490;
  moveto ((width / 2) - 100) 425;
  set_color (text_color palette);
  draw_string "Click on a button to choose a category.";
  ignore (wait_next_event [ Button_down ]);
  let category = choose_cat () in
  if category = "MENU" then
    let new_settings = cat_is_menu palette lb in
    choose_cat_or_menu width height score new_settings lb
  else
    play_game
      (extract (choose_category category))
      width height score settings

let start_game width height score_opt settings =
  let palette = settings.palette in
  clear palette;
  set_color (text_color palette);
  moveto ((width / 2) - 50) (2 * height / 3);
  set_text_size 50;
  draw_string "Welcome to Hangman!";
  if score_opt = None then
    let username =
      get_username width (background_color palette) (text_color palette)
    in
    let score = init_score username in
    choose_cat_or_menu width height score settings pretty_score_list
  else
    let score = extract_score score_opt in
    choose_cat_or_menu width height score settings pretty_score_list
