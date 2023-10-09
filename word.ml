open Random

type word_info = {
  word : string;
  len : int;
  num_unique : int;
}

exception InvalidWord of string

exception InvalidCharacter of char

let get_word w = w.word

let choose_word (a : string array) : string =
  Random.self_init ();
  let index = Random.int (Array.length a) in
  a.(index)

let remove_from_list lst a = List.filter (fun x -> x <> a) lst

let make_list w =
  let lst = ref [] in
  for i = 0 to String.length w - 1 do
    lst := w.[i] :: !lst
  done;
  remove_from_list (List.rev !lst) ' '

let unique_letters w =
  let w_lst = make_list w in
  let new_lst = List.sort_uniq compare w_lst in
  List.length new_lst

let get_word_info w =
  { word = w; len = String.length w; num_unique = unique_letters w }

let word_difficulty t = if t.num_unique <= 7 then 6 else 10

(* [make_blanks] turns [str] into a string of blanks only. A single
   blank is one underscore "_" followed by a single space " "*)
let make_blanks str =
  let s = ref "" in
  for i = 0 to String.length str - 1 do
    s := "_ " ^ !s
  done;
  !s

(* [word_blank str_list acc] returns a list of blanks for each word in
   [str_list] For example: [word_blank ["HELLO"] []] returns ["_ _ _ _
   _";] and [word_blank ["A DOG"] []] returns ["_ "; "_ _ _ ";]*)
let rec word_blank str_list acc =
  match str_list with
  | [] -> acc
  | h :: t -> make_blanks h :: word_blank t acc

let display_blanks w =
  let str_lst = String.split_on_char ' ' w.word in
  word_blank str_lst [] |> String.concat "  "

let check_correct c t =
  let ascii = Char.code c in
  if ascii >= 65 && ascii <= 90 then String.contains t.word c
  else raise (InvalidCharacter c)
