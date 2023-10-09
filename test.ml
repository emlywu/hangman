(* cloc --by-file --include-lang=OCaml . *)

(*************************** TEST PLAN ********************************)
(* For this Hangman program, test cases were designed and developed
   using black box testing the majority of the time. Especially in the
   early stages of developing this program, at any point a new function
   was made, around 5-10 test cases were made for each function in order
   to prove their functionality. In addition, almost every module of the
   program (Draw, Game, Gui, Menu, Score, Word) was inspected and for
   any functions that could be tested by OUnit, test cases were written.
   To mention an important facet of our testing, a large portion of this
   program used the Graphics library. Most of these functions resulted
   in type unit () and produced graphical output as opposed to values,
   and therefore were not testable in the typical OUnit fashion. For
   these functions, we tested Graphics by running our code and seeing
   what would happen if we changed inputs to functions we wrote.
   Basically, we were using randomized, aka fuzz testing, here. Graphics
   is a simple enough library to tell when something is going wrong, and
   taking the time to play around with all of its tools and see what
   works and what doesn't work helped solidify our understanding of the
   program's functionality. According to the CS 3110 textbook,
   Correctness here means that the program produces the right output
   according to a specification. In all of the .mli/.ml files, there are
   specification comments for functions that describe the function's
   precondition and postcondition, and any other useful information. The
   testing approaches found here in this testing suite for the program
   demonstrate the correctness of the system because the test cases and
   manual testing all check that the program produces the right output
   according to the specification. *)

open OUnit2
open Word
open WordLists
open Game
open Score
open Gui
open Menu

let print_string s = "\"" ^ s ^ "\""

let print_bytes b = Bytes.to_string b

let choose_word_test (name : string) (a : string array) : test =
  name >:: fun _ -> assert_equal (Array.mem (choose_word a) a) true

let word_difficulty_test
    (name : string)
    (w : string)
    (expected_guesses : int) : test =
  name >:: fun _ ->
  assert_equal (word_difficulty (get_word_info w)) expected_guesses

let display_blanks_test
    (name : string)
    (w : word_info)
    (expected_value : string) : test =
  name >:: fun _ ->
  assert_equal (display_blanks w) expected_value ~printer:print_string

let check_correct_test
    (name : string)
    (c : char)
    (t : word_info)
    (expected_value : bool) : test =
  name >:: fun _ -> assert_equal (check_correct c t) expected_value

let update_blanks_test
    (name : string)
    (c : char)
    (st : state)
    (word : string)
    (expected_value : bytes) : test =
  name >:: fun _ ->
  assert_equal
    (update_blanks c st word)
    expected_value ~printer:print_bytes

let make_char_list_test
    (name : string)
    (s : string)
    (expected_value : char list) : test =
  name >:: fun _ -> assert_equal (make_char_list s) expected_value

let find_index_test
    (name : string)
    c
    lst
    (acc : int)
    (expected_value : int) : test =
  name >:: fun _ -> assert_equal (find_index c lst acc) expected_value

let sum_list_test (name : string) (l : int list) (expected_value : int)
    : test =
  name >:: fun _ -> assert_equal (sum_list l) expected_value

let max_list_test (name : string) (l : int list) (expected_value : int)
    : test =
  name >:: fun _ -> assert_equal (max_list l) expected_value

(****************************************************************************)
(* ----------------------- SAMPLE WORD INFOS -----------------------*)

let w1 = get_word_info "EZRA CORNELL"

let w2 = get_word_info "JASMINE"

let w3 = get_word_info "MANGO TANGO"

let w4 = get_word_info "BOA CONSTRICTOR"

let w5 = get_word_info "HORCRUX"

let w6 = get_word_info "SOUTH KOREA"

(****************************************************************************)
let word_tests =
  [
    (* ---------------- CHOSE_WORD_TEST tests---------------------- *)
    choose_word_test "harry potter category"
      (get_word_list harry_potter);
    choose_word_test "countries category" (get_word_list countries);
    choose_word_test "crayola category" (get_word_list crayon_colors);
    choose_word_test "cornell category" (get_word_list cornell);
    choose_word_test "disney category" (get_word_list disney_characters);
    (* ---------------- WORD_DIFFICULTY_TEST tests---------------------- *)
    word_difficulty_test "easy word" "HELLO" 6;
    word_difficulty_test "easy word with many duplicates"
      "AABBBBCCCCDDE" 6;
    word_difficulty_test "hard word" "MICROWAVED" 10;
    word_difficulty_test "hard word with many duplicates"
      "MMMMMMICCCCCROWWWWAVEEEEEEDD" 10;
    word_difficulty_test "easy word with spaces" "HARRY POTT" 6;
    word_difficulty_test "hard word with spaces" "HARRY POTTER" 10;
    (* ---------------- DISPLAY_BLANKS_TEST tests ---------------------- *)
    display_blanks_test "input: 'APRICOT'"
      (get_word_info "APRICOT")
      "_ _ _ _ _ _ _ ";
    display_blanks_test "input: 'PETER PAN'"
      (get_word_info "PETER PAN")
      "_ _ _ _ _   _ _ _ ";
    display_blanks_test "input: 'WINNIE THE POOH'"
      (get_word_info "WINNIE THE POOH")
      "_ _ _ _ _ _   _ _ _   _ _ _ _ ";
    display_blanks_test "input: 'HERMIONE GRANGER'"
      (get_word_info "HERMIONE GRANGER")
      "_ _ _ _ _ _ _ _   _ _ _ _ _ _ _ ";
    display_blanks_test "input: 'LIGHTNING MCQUEEN'"
      (get_word_info "LIGHTNING MCQUEEN")
      "_ _ _ _ _ _ _ _ _   _ _ _ _ _ _ _ ";
    (* ---------------- CHECK_CORRECT_TEST tests ---------------------- *)
    check_correct_test "true case" 'Z' w1 true;
    check_correct_test "false case" 'P' w2 false;
    check_correct_test "another false case" 'Z' w5 false;
    ( "raise InvalidCharacter, although true" >:: fun _ ->
      assert_raises (InvalidCharacter 'o') (fun () ->
          check_correct 'o' w3) );
    ( "raise InvalidCharacter, although false" >:: fun _ ->
      assert_raises (InvalidCharacter 'm') (fun () ->
          check_correct 'm' w4) );
    ( "raise InvalidCharacter, not a letter" >:: fun _ ->
      assert_raises (InvalidCharacter '!') (fun () ->
          check_correct '!' w4) );
    ( "raise InvalidCharacter, tab" >:: fun _ ->
      assert_raises (InvalidCharacter ' ') (fun () ->
          check_correct ' ' w4) );
  ]

let s1 = init_state w1

let s2 = "MANGO TANGO" |> get_word_info |> init_state

let s3 = "FUZZY WUZZY" |> get_word_info |> init_state

let s4 = "COLLEGETOWN BAGELS" |> get_word_info |> init_state

let s5 = "AFGHANISTAN" |> get_word_info |> init_state

let game_tests =
  [
    (* ---------------- UPDATE_BLANKS_TEST tests ---------------------- *)
    update_blanks_test "input: c = 'A'; st = s1; word = 'EZRA CORNELL'"
      'A' s1 "EZRA CORNELL"
      (Bytes.of_string "_ _ _ A   _ _ _ _ _ _ _ ");
    update_blanks_test "input: c = 'A'; st = s2; word = 'MANGO TANGO'"
      'A' s2 "MANGO TANGO"
      (Bytes.of_string "_ A _ _ _   _ A _ _ _ ");
    update_blanks_test "input: c = 'Z'; st = s3; word = 'FUZZY WUZZY'"
      'Z' s3 "FUZZY WUZZY"
      (Bytes.of_string "_ _ Z Z _   _ _ Z Z _ ");
    update_blanks_test "guessing a letter that has already been guessed"
      'Z' s3 "FUZZY WUZZY"
      (Bytes.of_string "_ _ Z Z _   _ _ Z Z _ ");
    update_blanks_test
      "input: c = 'A'; st = s5; word = 'COLLEGETOWN BAGELS'" 'L' s4
      "COLLEGETOWN BAGELS"
      (Bytes.of_string "_ _ L L _ _ _ _ _ _ _   _ _ _ _ L _ ");
    update_blanks_test "input: c = 'A'; st = s5; word = 'AFGHANISTAN'"
      'A' s5 "AFGHANISTAN"
      (Bytes.of_string "A _ _ _ A _ _ _ _ A _ ");
    update_blanks_test "guessing another letter" 'N' s5 "AFGHANISTAN"
      (Bytes.of_string "A _ _ _ A N _ _ _ A N ");
  ]

let other_tests =
  [
    (* ---------------- MAKE_CHAR_LIST tests ---------------------- *)
    make_char_list_test "one element string" "h" [ 'H' ];
    make_char_list_test "normal word lowercase" "banana"
      [ 'B'; 'A'; 'N'; 'A'; 'N'; 'A' ];
    make_char_list_test "normal word uppercase" "TANGERINE"
      [ 'T'; 'A'; 'N'; 'G'; 'E'; 'R'; 'I'; 'N'; 'E' ];
    make_char_list_test "empty string" "" [];
    make_char_list_test "word with one space" "hello sir"
      [ 'H'; 'E'; 'L'; 'L'; 'O'; 'S'; 'I'; 'R' ];
    make_char_list_test "word with spaces" "my name is jeff"
      [ 'M'; 'Y'; 'N'; 'A'; 'M'; 'E'; 'I'; 'S'; 'J'; 'E'; 'F'; 'F' ];
    (* ---------------- FIND_INDEX tests ---------------------- *)
    find_index_test "regular list, first index" 'b'
      [ 'b'; 'c'; 'f'; 'r'; 'p'; 's' ]
      0 0;
    find_index_test "regular list, different index " 'p'
      [ 'b'; 'c'; 'f'; 'r'; 'p'; 's' ]
      0 4;
    find_index_test "regular list, last index" 's'
      [ 'b'; 'c'; 'f'; 'r'; 'p'; 's' ]
      0 5;
    (* ---------------- SUM_LIST tests ---------------------- *)
    sum_list_test "no elements" [] 0;
    sum_list_test "some elements" [ 6; 4; 8; 0 ] 18;
    sum_list_test "all zero elements" [ 0; 0; 0; 0; 0 ] 0;
    sum_list_test "negative elements" [ -6; -4; -8 ] (-18);
    sum_list_test "negative and positive elements"
      [ -6; -4; -8; 10; 5; 3; -1; -2 ]
      (-3);
    sum_list_test "negs, pos, zeroes"
      [ -6; -4; -8; 5; 6; 9; 0; 1; 2; -3 ]
      2;
    (* ---------------- MAX_LIST tests ---------------------- *)
    max_list_test "no elements" [] 0;
    max_list_test "some elements" [ 6; 4; 8; 0 ] 8;
    max_list_test "all zero elements" [ 0; 0; 0; 0; 0 ] 0;
    max_list_test "negative and positive elements"
      [ -6; -4; -8; 10; 5; 3; -1; -2 ]
      10;
    max_list_test "repeat elements" [ 1; 3; 3; 6; 9; 9 ] 9;
    max_list_test "repeat elements in random"
      [ 11; 1; 3; 15; 3; 4; 6; 9 ]
      15;
  ]

let suite =
  "test suite for A2"
  >::: List.flatten [ word_tests; game_tests; other_tests ]

let _ = run_test_tt_main suite
