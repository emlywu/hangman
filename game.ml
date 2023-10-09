open Word

exception IncorrectGuess of char

exception IncorrectWord of string

type state = {
  mutable progress : bytes;
  mutable guesses_left : int;
  mutable letters_guessed : char list;
  mutable incorrect_guesses : char list;
}

let init_state (w : word_info) =
  {
    progress = Word.display_blanks w |> Bytes.of_string;
    guesses_left = Word.word_difficulty w;
    letters_guessed = [];
    incorrect_guesses = [];
  }

(* [fill_in str c] returns a new string with the letter s*)
let rec fill_in c word st ind =
  let current = st.progress in
  match ind with
  | [] -> st.progress
  | h :: t ->
      for i = 0 to Bytes.length current - 1 do
        if i = h * 2 then Bytes.set current i c
      done;
      fill_in c word st t

let update_blanks c st word =
  try
    if check_correct c (get_word_info word) then (
      let indices = ref [] in
      for i = 0 to String.length word - 1 do
        if word.[i] = c then indices := i :: !indices
      done;
      st.letters_guessed <- c :: st.letters_guessed;
      List.sort compare !indices |> fill_in c word st)
    else (
      st.guesses_left <- st.guesses_left - 1;
      st.incorrect_guesses <- c :: st.incorrect_guesses;
      st.letters_guessed <- c :: st.letters_guessed;
      raise (IncorrectGuess c))
  with InvalidCharacter c ->
    st.guesses_left <- st.guesses_left - 1;
    raise (InvalidCharacter c)

let get_blanks st = Bytes.to_string st.progress

let get_guesses st = st.guesses_left

let get_letters st = st.letters_guessed

let guess_word str correctstr =
  if str <> correctstr then raise (IncorrectWord str) else ()

let get_incorrect st = st.incorrect_guesses
