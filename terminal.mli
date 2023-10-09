(** The terminal implementation of the game. *)

open WordLists
open Word
open Game

(** [choose_category c] is the string array associated with category
    name [c], or None if [c] is not a valid category name. *)
val choose_category : string -> string array option

(** [check_answer word guess_str] is true if [word] is the same word as
    [guess_str] and false otherwise. *)
val check_answer : string -> string -> bool

(** [print_guessed state] prints the previous guesses stored in [state]. *)
val print_guessed : state -> string

(** [make_guess word state] takes in the current state and word info,
    prompts the player for a guess, and updates the state based off of
    that guess. *)
val make_guess : string -> state -> unit

(** [play_game cat_name category] selects word from [category] and
    starts the game. *)
val play_game : string -> string array -> unit
