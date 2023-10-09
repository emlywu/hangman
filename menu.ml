open Graphics
open Draw
open Word

let lt_green = rgb 129 194 118

let dk_green = rgb 83 138 74

let md_green = rgb 106 168 96

let lt_gray = rgb 232 232 232

let med_gray = rgb 194 194 194

let dk_gray = rgb 150 150 150

type palette = {
  background : color;
  button_light : color;
  button_medium : color;
  button_dark : color;
  text : color;
  drawing : color;
}

let standard_palette =
  {
    background = rgb 255 255 255;
    button_light = lt_green;
    button_medium = md_green;
    button_dark = dk_green;
    text = rgb 0 0 0;
    drawing = rgb 0 0 0;
  }

let cornell_palette =
  {
    background = rgb 179 27 26;
    button_light = rgb 194 129 76;
    button_medium = rgb 136 87 46;
    button_dark = rgb 94 57 31;
    text = rgb 255 255 255;
    drawing = rgb 0 0 0;
  }

let sunset_park =
  {
    background = rgb 143 198 203;
    button_light = rgb 251 215 183;
    button_medium = rgb 236 180 191;
    button_dark = rgb 198 172 199;
    text = rgb 255 240 150;
    drawing = rgb 49 106 107;
  }

let ithaca_is_gorges =
  {
    background = rgb 116 167 48;
    button_light = rgb 234 237 206;
    button_medium = rgb 236 223 95;
    button_dark = rgb 236 202 17;
    text = rgb 172 255 77;
    drawing = rgb 67 105 4;
  }

let cayugas_waters =
  {
    background = rgb 165 162 232;
    button_light = rgb 140 134 252;
    button_medium = rgb 73 66 196;
    button_dark = rgb 19 12 153;
    text = rgb 148 246 255;
    drawing = rgb 5 0 119;
  }

let background_color p = p.background

let button_light_color p = p.button_light

let button_med_color p = p.button_medium

let button_dark_color p = p.button_dark

let text_color p = p.text

let drawing_color p = p.drawing

(* [draw_color_square x x_offset y color] is a helper function for
   [draw_palette_row] that draws each square a certain color for the
   given color palette *)
let draw_color_square x x_offset y color =
  draw_rect (x + x_offset) y 40 40;
  set_color color;
  fill_rect (x + x_offset) y 40 40;
  set_color black

(* [draw_palette_row p x y name] is a helper function for [make_palette]
   that draws one row of the palette choices, associated with palette
   [p]. *)
let draw_palette_row p x y name =
  draw_button lt_gray med_gray dk_gray x y 100 45 10 10 name;
  set_line_width 5;
  draw_color_square x 145 y p.background;
  draw_color_square x 216 y p.button_light;
  draw_color_square x 287 y p.button_medium;
  draw_color_square x 358 y p.button_dark;
  draw_color_square x 429 y p.text;
  draw_color_square x 500 y p.drawing;
  set_color black;
  set_line_width 0

let rec choose_palette () =
  ignore (wait_next_event [ Button_down ]);
  if mouse_in_button 225 175 100 45 then cayugas_waters
  else if mouse_in_button 225 280 100 45 then ithaca_is_gorges
  else if mouse_in_button 225 385 100 45 then sunset_park
  else if mouse_in_button 225 490 100 45 then cornell_palette
  else choose_palette ()

let make_palette () =
  clear_graph ();
  set_line_width 3;
  set_color black;
  draw_rect 200 140 600 420;
  set_line_width 0;
  draw_palette_row cayugas_waters 225 175 "CAYUGA'S WATERS";
  draw_palette_row ithaca_is_gorges 225 280 "ITHACA IS GORGES";
  draw_palette_row sunset_park 225 385 "SUNSET PARK";
  draw_palette_row cornell_palette 225 490 "CORNELL"

let exit_window_button x y =
  set_color black;
  set_line_width 5;
  draw_rect x y 30 30;
  set_color red;
  fill_rect x y 30 30;
  set_line_width 2;
  set_color white;
  draw_left_eye (x + 35) (y - 5) 50;
  set_color black;
  set_line_width 0

let back_button x y =
  set_color med_gray;
  fill_rect x y 40 25;
  set_color black;
  set_line_width 2;
  draw_rect x y 40 25;
  moveto (x + 8) (y + 7);
  set_color black;
  draw_string "BACK";
  set_line_width 0

let make_instructions () =
  clear_graph ();
  set_line_width 3;
  set_color black;
  draw_rect 200 140 600 420;
  set_line_width 0;
  exit_window_button 210 515;
  back_button 210 475;
  moveto 450 400;
  draw_string "WELCOME TO HANGMAN!";
  moveto 300 350;
  draw_string "1. Select a category.";
  moveto 300 325;
  draw_string "2. Enter a letter you guess might be in the word.";
  moveto 300 300;
  draw_string
    "3. Attempt to guess the word before all of the man's limbs appear!"

let show_menu () =
  set_line_width 3;
  set_color black;
  draw_rect 200 140 600 420;
  set_line_width 0;
  draw_button lt_gray med_gray dk_gray 450 175 100 45 10 10
    "Instructions";
  draw_button lt_gray med_gray dk_gray 450 280 100 45 10 10
    "Color Palette";
  draw_button lt_gray med_gray dk_gray 450 385 100 45 10 10
    "Leaderboard";
  draw_button lt_gray med_gray dk_gray 450 490 100 45 10 10
    "Show Keyboard";
  exit_window_button 210 515

let letters_r1 = [ 'Q'; 'W'; 'E'; 'R'; 'T'; 'Y'; 'U'; 'I'; 'O'; 'P' ]

let letters_r2 = [ 'A'; 'S'; 'D'; 'F'; 'G'; 'H'; 'J'; 'K'; 'L' ]

let letters_r3 = [ 'Z'; 'X'; 'C'; 'V'; 'B'; 'N'; 'M'; '?' ]

(* [draw_row_1 x y] is a helper function for [show_keyboard]. It draws
   the top row of the QUERTY keyboard (letters Q through P) starting at
   position (x, y) *)
let draw_row_1 x y =
  moveto x y;
  for i = 0 to List.length letters_r1 - 1 do
    draw_char (List.nth letters_r1 i);
    moveto (current_x () + 20) y
  done

(* [draw_row_2 x y] is a helper function for [show_keyboard]. It draws
   the middle row of the QUERTY keyboard (letters A through L) starting
   at position (x, y) *)
let draw_row_2 x y =
  moveto x y;
  for i = 0 to List.length letters_r2 - 1 do
    draw_char (List.nth letters_r2 i);
    moveto (current_x () + 20) y
  done

(* [draw_row_3 x y] is a helper function for [show_keyboard]. It draws
   the bottom row of the QUERTY keyboard (letters Z through M) starting
   at position (x, y) *)
let draw_row_3 x y =
  moveto x y;
  for i = 0 to List.length letters_r3 - 1 do
    draw_char (List.nth letters_r3 i);
    moveto (current_x () + 20) y
  done

(* [find_row c] finds which row character [c] is located in *)
let find_row c =
  if List.mem c letters_r1 then letters_r1
  else if List.mem c letters_r2 then letters_r2
  else if List.mem c letters_r3 then letters_r3
  else raise (InvalidCharacter c)

(* [find_index c lst acc] finds the index of char [c] in the list of
   chars for the row that [c] is in*)
let rec find_index c lst acc =
  match lst with
  | [] -> -1
  | h :: t -> if c = h then acc else find_index c t (acc + 1)

(* [find_letter c x y] finds the coordinates of a letter [c], when the
   keyboard is drawn starting at (x, y)*)
let find_letter c x y =
  let letters_rx = find_row c in
  let c_index = find_index c letters_rx 0 in
  if letters_rx = letters_r1 then (x + (c_index * 20), y)
  else if letters_rx = letters_r2 then (x + 15 + (c_index * 20), y - 30)
  else (x + 30 + (c_index * 20), y - 60)

let show_keyboard b x y color =
  if b then (
    set_color color;
    draw_rect (x - 20) (y - 70) 275 90;
    draw_row_1 x y;
    draw_row_2 (x + 15) (y - 30);
    draw_row_3 (x + 30) (y - 60))
  else ()

(* [erase_char c x y] erases a char [c] at position (x, y)*)
let erase_char c x y =
  moveto x y;
  set_color white;
  draw_char c

(* helper function for [find_row_by_pos]; determines whether the user
   has clicked on a letter in row 1*)
let r1_conditions mouse_y = mouse_y > 143 && mouse_y <= 165

(* helper function for [find_row_by_pos]; determines whether the user
   has clicked on a letter in row 2*)
let r2_conditions mouse_y = mouse_y > 113 && mouse_y < 144

(* helper function for [find_row_by_pos]; determines whether the user
   has clicked on a letter in row 3*)
let r3_conditions mouse_y = mouse_y >= 85 && mouse_y <= 113

(* [find_row_by_pos x y mouse_x mouse_y] finds the row that a mouse at
   (mouse_x, mouse_y) is hovering over, on a keyboard drawn at (x,y)*)
let find_row_by_pos mouse_y =
  if r1_conditions mouse_y then "r1"
  else if r2_conditions mouse_y then "r2"
  else "r3"

(* [row_1_chars] takes in the x position of the user's mouse and detects
   what letter they clicked*)
let row_1_chars x =
  if x >= 48 && x <= 55 then 'Q'
  else if x >= 70 && x <= 80 then 'W'
  else if x >= 95 && x <= 105 then 'E'
  else if x >= 120 && x <= 140 then 'R'
  else if x >= 150 && x <= 160 then 'T'
  else if x >= 170 && x <= 185 then 'Y'
  else if x >= 195 && x <= 215 then 'U'
  else if x >= 225 && x <= 235 then 'I'
  else if x >= 240 && x <= 265 then 'O'
  else if x >= 275 && x <= 285 then 'P'
  else ' '

(* [row_2_chars] takes in the x position of the user's mouse and detects
   what letter they clicked*)
let row_2_chars x =
  if x >= 60 && x <= 80 then 'A'
  else if x >= 85 && x <= 95 then 'S'
  else if x >= 110 && x <= 120 then 'D'
  else if x >= 135 && x <= 150 then 'F'
  else if x >= 160 && x <= 170 then 'G'
  else if x >= 190 && x <= 200 then 'H'
  else if x >= 215 && x <= 225 then 'J'
  else if x >= 240 && x <= 250 then 'K'
  else if x >= 265 && x <= 275 then 'L'
  else ' '

(* [row_3_chars] takes in the x position of the user's mouse and detects
   what letter they clicked*)
let row_3_chars x =
  if x >= 70 && x <= 85 then 'Z'
  else if x >= 95 && x <= 115 then 'X'
  else if x >= 120 && x <= 140 then 'C'
  else if x >= 145 && x <= 165 then 'V'
  else if x >= 175 && x <= 190 then 'B'
  else if x >= 200 && x <= 220 then 'N'
  else if x >= 225 && x <= 245 then 'M'
  else if x >= 250 && x <= 270 then '?'
  else ' '

(* [get_char row mouse_x] takes the row that the user clicked in and the
   x-position of their mouse and returns the letter they clicked on*)
let get_char row mouse_x =
  if row = "r1" then row_1_chars mouse_x
  else if row = "r2" then row_2_chars mouse_x
  else row_3_chars mouse_x

let rec detect_letter x y =
  ignore (wait_next_event [ Button_down ]);
  let mouse_x = fst (mouse_pos ()) in
  let mouse_y = snd (mouse_pos ()) in
  let get_row = find_row_by_pos mouse_y in
  let guess = get_char get_row mouse_x in
  if guess = ' ' then detect_letter x y else guess
