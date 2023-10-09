(** The type and functions associated with the GUI and its logic. *)

open Game
open Word
open WordLists
open Score
open Menu

(** type [settings] keeps track of whether the user wants the keyboard
    to be displayed on the screen and the color palette choice. *)
type settings

(** [init_settings] is a settings object initialized to not show the
    keys and have the palette be the standard palette. *)
val init_settings : settings

(** [new_round s] takes the most recent settings object [s] and updates
    it to change the keyboard setting to false, but keeps the same color
    palette. *)
val new_round : settings -> settings

(** [Start_Over] is raised when the user would like to play again. It
    also provides the most recent score and settings objects. *)
exception Start_Over of score * settings

(** type [status] indicates whether a limb should be drawn, should not
    be drawn, or is drawn. *)
type status

(** type [body_record] stores the game's difficulty, whether or not the
    man needs to be shifted (if a hat will be used, based off of the
    difficulty), and the drawing status of each limb. *)
type body_record

(** [draw_score s p] draws the current score in the upper left corner of
    the screen. *)
val draw_score : score -> palette -> unit

(** [draw_round_score s p] draws the user's total score for the round. *)
val draw_round_score : score -> palette -> unit

(** [init_guesses_box x y w h] draws a box of width [w] and height [h]
    that contains the guesses made by the user with the bottom left
    corner at coordinate position ([x], [y])*)
val init_guesses_box : int -> int -> int -> int -> palette -> unit

(** [draw_guesses x y c st] takes coordinate positions ([x], [y]), a
    character [c], and a state [st], and draws the incorrect guess [c]
    in the guesses box. *)
val draw_guesses : int -> int -> char -> state -> unit

(** [new_guess c st] takes a state [st] and a character [c] and checks
    if [c] has been guessed before. If it has not been guessed, then [c]
    is a new guess and [new_guess] will return true. Else, false will be
    returned. *)
val new_guess : char -> state -> bool

(** [check_answer word guess_str] checks if the word has been guessed. *)
val check_answer : string -> string -> bool

(** [game_over st word] detects when the game should end. The game
    should end if the whole word has been guessed or if the user has run
    out of guesses. *)
val game_over : state -> string -> bool

(** [display_win_lose st word w score p] displays if the user has won
    the game or lost the game. *)
val display_win_lose :
  state -> string -> int -> score -> palette -> unit

(** [instatiate_body difficulty r] is a record of type body_record that
    instantiates the record to indicate whether each limb should be
    drawn or not based off of [difficulty], and how big the shift should
    be based off of [r]. *)
val instantiate_body : bool -> int -> body_record

(** [draw_next r] is an updated body_record object with the next limb
    drawn on the screen and its status in the record updated. *)
val draw_next : body_record -> palette -> body_record

(** [choose_category c] is a string array option, where the string array
    is a list of words associated with category [c]. *)
val choose_category : string -> string array option

(** [make_char_list str] will convert [str] from a string to a list of
    that string's character's with spaces removed and all characters are
    uppercase. Helper function for [reveal_letter]. Note: this function
    was added to the public documentation so that it could be referenced
    in our testing suite. *)
val make_char_list : string -> char list

(** [reveal_letter word state] will reveal a letter from the hidden
    [word] in the current [state]. *)
val reveal_letter : string -> state -> char

(** [make_guess word state width height word_width r score] prompts the
    user for a guess, checks if it is correct, and updates the screen
    and [score] accordingly. *)
val make_guess :
  string ->
  state ->
  int ->
  int ->
  int ->
  body_record ->
  score ->
  settings ->
  unit

(** [play_game category w h score palette] starts the game by choosing a
    word from [category], drawing the gallows, initializing the state
    and word_info objects, clearing the screen, and drawing the first
    blanks. *)
val play_game : string array -> int -> int -> score -> settings -> unit

(** [play_again] asks the player if they would like to restart the game
    after they win or lose, and closes the window if they do not click
    the play again button. *)
val play_again : score -> settings -> unit

(** [start_game width height score_opt palette] initializes the screen
    with the category buttons and welcomes the player. Once the player
    chooses their category, it starts the game. *)
val start_game : int -> int -> score option -> settings -> unit
