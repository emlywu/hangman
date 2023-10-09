(** The type and functions associated with keeping track of the player's
    score and the leaderboard. *)

(** The abstract type of values representing a player's score. *)
type score

(** The player's username. Will be the same across each round. *)
type username = string

(** A list of all of the scores a player got in a row while winning. *)
type round_scores = int list

(** The highest number of rounds a player has won consecutively. *)
type max_streak = int

(** The maximum score a player has gotten on any individual round. *)
type highest_round_score = int

(** The highest total score the player has gotten while on a streak. *)
type max_total_score = int

(** A list of scores representing at most the 10 best scores. *)
type leaderboard = score list

(** [init_score n] is an object of type score with [n] as the username. *)
val init_score : string -> score

val get_high_score : score -> int

(** [update_score s i win] is a new score object that will be created
    after each round ends. [i] is the player's score in the most recent
    round and [win] is true if the player won and false if they lost. If
    [i] is higher than [highest_round_score], [i] replaces the current
    value in the field. If [win] is true, [i] is appended onto
    [round_scores] in [score] and [streak] increases by one. If [win] is
    false, [streak] becomes 1, [total_score] becomes the sum of
    [round_scores] if this total would be higher than the current
    [total_scores], and [round_scores] is cleared. At the end of each
    round, the current score for the round will be reset to 0. *)
val update_score : score -> int -> bool -> score

(** [final_score s] is [s] with the streak updated to the highest
    streak. *)
val final_score : score -> score

(** [init_round_score w s] returns a score object with
    current_round_score intiailized to 1000 if the word [w] is difficult
    and 800 if [w] is easy. *)
val init_round_score : Word.word_info -> score -> unit

(** [sum_list l] is the sum of the values in [l]. Note: this function
    was added to the public documentation so that it could be referenced
    in our testing suite. *)
val sum_list : int list -> int

(** [max_list l] is the max of the values in [l]. Note: this function
    was added to the public documentation so that it could be referenced
    in our testing suite. *)
val max_list : int list -> int

(** [update_round_score c w s] is the updated score object with the
    current score [crnt_score] after a guess [c] has been made for word
    [w]. *)
val update_round_score : char -> string -> score -> unit

(** [update_after_hint s] updates the user's score after a hint has been
    used. *)
val update_after_hint : score -> unit

(** [get_current_score s] returns the current_round_score of score
    object [s]. *)
val get_current_score : score -> int

(** [update_leaderboard l s] is [l] with [s] added in the correct
    position. If [s] has [max_total_score] larger than at least one
    element of leaderboard [l], [s] is placed in the correct position on
    the leaderboard, where position 0 has the highest score of all
    elements and position 9 has the lowest. If [s] does not have a
    higher total score than any of the elements, [l] is returned. The
    output leaderboard should have at most 10 elements. *)
val update_leaderboard : leaderboard -> score -> leaderboard

(** [draw_lb_lines width height lb] takes in the [width] and [height] of
    the screen and a leaderboard [lb] in order to draw the leaderboard
    with all of the pieces put together. *)
val draw_lb_lines : int -> int -> leaderboard -> unit

(** [draw_lb_text height x1 x2 x3 x4 x5 spacer] draws the leaderboard
    titles as a helper function for [draw_lb_lines]. *)
val draw_lb_text :
  float -> float -> float -> float -> float -> float -> float -> unit

(** [draw_player_stats leaderboard height y_vert spacer x2 x3 x4 x5]
    takes in the leaderboard and height of the window in order to draw
    the numbers and information associated with scores. The other inputs
    are simply for stylistic purposes with shifting. *)
val draw_player_stats :
  score list ->
  float ->
  float ->
  float ->
  float ->
  float ->
  float ->
  float ->
  unit

(** [draw_lb_ranks x1 height y_vert spacer] draws the ranks from 1 to 10
    on the leaderboard and uses the height of the leaderboard and other
    size inputs to space them correctly. *)
val draw_lb_ranks : float -> float -> float -> float -> unit

(** [read_leaderboard filename] will convert the text in a [filename]
    text file into a string list. *)
val read_leaderboard : string -> string list

(** [print_scores oc score_list] is an essential helper function for
    write_leaderboard, it actually does the printing neatly. *)
val print_scores : out_channel -> score list -> unit

(** [write_leaderboard leaderboard] will take the [leaderboard] and
    write all the information to the textfile used in the game. *)
val write_leaderboard : leaderboard -> unit

(** [pretty_score_list] reads in and stores the scores from practice.txt *)
val pretty_score_list : score list
