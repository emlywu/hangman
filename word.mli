(** The abstract type of values representing words and their qualities. *)
type word_info

exception InvalidWord of string

exception InvalidCharacter of char

(* The word associated with a word_info object. *)
val get_word : word_info -> string

(** [ choose_word a ] is a random word from the array a. Requires: a is
    a nonempty string array. *)
val choose_word : string array -> string

(** [ get_word_info w ] is an object of type t that contains the word
    [w], the length of [w], and the number of unique letters in [w].
    Spaces may be in the word, but do not count as unique letters and
    should not be included in the length or number of unique letters.
    Raises [InvalidWord w] if the word contains characters other than
    uppercase letters and spaces. *)
val get_word_info : string -> word_info

(** [ word_difficulty w ] is an integer representing the number of
    guesses based off of the number of unique letters in the word that
    [w] represents. If the word has 7 or fewer unique letters, the
    function should return 6. Else, return 10. *)
val word_difficulty : word_info -> int

(** [ display_blanks w ] is a string of n underscores “_”, where n
    is the length of the word [w]. The string should also have spaces
    wherever spaces may exist in the original input. *)
val display_blanks : word_info -> string

(** [ check_correct c t ] checks if char c is in the word represented by
    t, and returns true or false. Raises [InvalidCharacter c] if c is
    anything but an uppercase letter. *)
val check_correct : char -> word_info -> bool

(** [unique_letters w] returns the number of unique letters in a string
    [w]. *)
val unique_letters : string -> int

(** [remove_from_list lst a] returns a list with all the elements are
    equal to [a] removed. *)
val remove_from_list : 'a list -> 'a -> 'a list

(** [make_list w] returns a list composed of the characters of string
    [w] *)
val make_list : string -> char list
