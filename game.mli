(** The type and functions associated with tracking the player's
    progress in the game.*)

(** The abstract type of values representing the state a user is during
    the game. *)
type state

exception IncorrectGuess of char

exception IncorrectWord of string

(** [init_state w] takes a word_info [w] and initializes the game state *)
val init_state : Word.word_info -> state

(** [ update_blanks c st wd ] takes a string of revealed letters and
    unrevealed letters and returns a string where all instances of
    character [c] in [wd], and any other previously revealed characters,
    are revealed. Unrevealed letters are represented by '_'. Requires:
    [c] is an uppercase character that is in the word represented by
    [wd]. *)
val update_blanks : char -> state -> string -> bytes

(** [get_blanks st] takes a state [st] and returns the string that
    represents the progress a user has made so far. *)
val get_blanks : state -> string

(** [get_guesses st] takes a state [st] and returns the number of
    guesses that the user has left. *)
val get_guesses : state -> int

(** [get_letters st] takes a state [st] and returns the list of
    characters containing all of the letters that the player has guessed
    so far. *)
val get_letters : state -> char list

(** [guess_word str correctstr] raises [IncorrectWord str] if the guess
    [str] is incorrect, and does nothing otherwise. *)
val guess_word : string -> string -> unit

(** [get_incorrect st] takes a state [st] and returns the list of
    incorrect characters the player has guessed so far. *)
val get_incorrect : state -> char list
