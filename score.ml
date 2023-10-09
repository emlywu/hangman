open Word
open Graphics

type username = string

type round_scores = int list

type max_streak = int

type highest_round_score = int

type max_total_score = int

type score = {
  username : string;
  round_scores : int list;
  mutable current_round_score : int;
  streak_list : int list;
  streak : int;
  max_streak : int;
  highest_round_score : int;
  total_scores_list : int list;
  total_score : int;
  max_total_score : int;
}

type leaderboard = score list

let init_score n =
  {
    username = n;
    round_scores = [];
    current_round_score = 0;
    streak = 1;
    streak_list = [];
    max_streak = 1;
    highest_round_score = 0;
    total_scores_list = [];
    total_score = 0;
    max_total_score = 0;
  }

let get_high_score s = s.highest_round_score

let sum_list l =
  let rec helper acc l =
    match l with [] -> acc | h :: t -> helper (acc + h) t
  in
  helper 0 l

let max_list l =
  let rec helper max l =
    match l with
    | [] -> max
    | h :: t -> if h > max then helper h t else helper max t
  in
  helper 0 l

(* [win_score_obj sc_obj n_high n_streak n_round_scores n_total] takes a
   score object [sc_obj] and returns the appropriate updated score
   object if the player has won the round. *)
let win_score_obj sc_obj n_high n_streak n_round_scores n_total =
  let n_streak_lst = n_streak :: sc_obj.streak_list in
  {
    sc_obj with
    highest_round_score = n_high;
    current_round_score = 0;
    streak = n_streak;
    streak_list = n_streak_lst;
    max_streak = max_list n_streak_lst;
    round_scores = n_round_scores;
    total_score = n_total;
    max_total_score = n_total;
  }

(* [lose_score_obj sc_obj n_high n_streak_lst n_total_lst n_total_max
   n_max_streak] takes a score object [sc_obj] and returns the
   appropriate updated score object if the player has lost the round. *)
let lose_score_obj
    sc_obj
    n_high
    n_streak_lst
    n_total_lst
    n_total_max
    n_max_streak =
  {
    sc_obj with
    highest_round_score = n_high;
    current_round_score = 0;
    streak = 1;
    total_score = 0;
    round_scores = [];
    streak_list = n_streak_lst;
    total_scores_list = n_total_lst;
    max_streak = n_max_streak;
    max_total_score = n_total_max;
  }

let update_score s i win =
  let curr_round_scores = s.round_scores in
  let curr_streak = s.streak in
  let new_high =
    if i > s.highest_round_score then i else s.highest_round_score
  in
  if win then
    let new_streak = curr_streak + 1 in
    let new_round_scores = i :: curr_round_scores in
    let new_total_score = sum_list new_round_scores in
    win_score_obj s new_high new_streak new_round_scores new_total_score
  else
    let new_streak_list = s.streak :: s.streak_list in
    let new_total_scores_list = s.total_score :: s.total_scores_list in
    let new_max_streak = max_list new_streak_list in
    let new_total_max = max_list new_total_scores_list in
    lose_score_obj s new_high new_streak_list new_total_scores_list
      new_total_max new_max_streak

(* [final_score s] is [s] with the streak updated to the highest streak. *)
let final_score s =
  let streak_list = s.streak :: s.streak_list in
  let final_max_streak = max_list streak_list in
  { s with max_streak = final_max_streak }

let init_round_score w s =
  let d = if word_difficulty w = 6 then false else true in
  s.current_round_score <- (if d then 1000 else 800)

(* [check_correct c w] takes a character [c] and a word [w] and checks
   if the [c] is in [w]*)
let check_correct_ws c w =
  let ascii = Char.code c in
  if ascii >= 65 && ascii <= 90 then String.contains w c
  else raise (InvalidCharacter c)

let get_current_score s = s.current_round_score

(* [should_uodate s] takes the score [s] and if updating the score will
   make the score negative, then returns false. Otherwise, it returns
   true *)
let should_update s =
  let current_s = get_current_score s in
  if current_s - 50 < 0 || current_s - 100 < 0 then false else true

let update_round_score c w s =
  if should_update s then
    if not (check_correct_ws c w) then
      s.current_round_score <- s.current_round_score - 50
    else s.current_round_score <- s.current_round_score

let update_after_hint s =
  if should_update s then
    s.current_round_score <- s.current_round_score - 100
  else s.current_round_score <- s.current_round_score

(* [extract_round_scores l] is a list of the [max_total_score] field
   from each score in leaderboard [l]. *)
let extract_round_scores l =
  let rec helper l acc =
    match l with
    | [] -> acc
    | h :: t -> helper t (h.max_total_score :: acc)
  in
  helper l []

(* [add_to_lst i lst] is true if [i] is bigger than at least one element
   of [lst] and false otherwise. If [lst] is empty to begin with then
   [add_to_lst i lst] is true for all [i].*)
let add_to_list i lst =
  if lst = [] then true
  else
    let rec helper i lst =
      match lst with
      | [] -> false
      | h :: t -> if i > h then true else helper i t
    in
    helper i lst

(* [compare_scores s1 s2] is 0 if s1.max_total_score =
   s2.max_total_score, negative if the first is smaller, and positive
   otherwise. *)
let compare_scores s1 s2 =
  if s1.max_total_score = s2.max_total_score then 0
  else if s1.max_total_score > s2.max_total_score then 1
  else -1

(* [truncate l] is [l] if [l] has 10 elements or less, otherwise returns
   the first 10 elements of [l]. [truncate] assumes that [l] is sorted
   from lowest to highest, so the last 10 elements are returned (not the
   first 10). *)
let truncate l =
  if List.length l <= 10 then l
  else
    let arr = Array.of_list l in
    let length = List.length l in
    let trunc_arr = Array.sub arr (length - 10) 10 in
    Array.to_list trunc_arr

let update_leaderboard l s =
  let round_scores = extract_round_scores l in
  let new_score = s.max_total_score in
  if add_to_list new_score round_scores then
    let concat = s :: l in
    let l' = List.sort compare_scores concat in
    truncate l'
  else l

(****************** LEADERBOARD READING & WRITING ***********************)
(************************************************************************)
(************************************************************************)

let read_leaderboard filename =
  let lines = ref [] in
  let channel = open_in filename in
  try
    while true do
      let add_to_lines = input_line channel :: !lines in
      lines := add_to_lines
    done;
    !lines
  with End_of_file ->
    close_in channel;
    let final = !lines in
    List.rev final

(* [make_score i] creates a score object out of the [i] to [i+3] fields
   in the leaderboard text file. *)
let make_score i =
  let arr = Array.of_list (read_leaderboard "leaderboard.txt") in
  {
    username = arr.(i);
    round_scores = [];
    current_round_score = 0;
    streak_list = [];
    streak = 0;
    max_streak = int_of_string arr.(i + 1);
    highest_round_score = int_of_string arr.(i + 2);
    total_scores_list = [];
    total_score = 0;
    max_total_score = int_of_string arr.(i + 3);
  }

let pretty_score_list =
  [
    make_score 0;
    make_score 4;
    make_score 8;
    make_score 12;
    make_score 16;
    make_score 20;
    make_score 24;
    make_score 28;
    make_score 32;
    make_score 36;
  ]

let rec print_scores oc score_list =
  match score_list with
  | [] -> ()
  | h :: t -> (
      match h with
      | c ->
          let u, max_s, max_r, max_t =
            ( c.username,
              c.max_streak,
              c.highest_round_score,
              c.max_total_score )
          in
          Printf.fprintf oc "%s\n%d\n%d\n%d\n" u max_s max_r max_t;
          print_scores oc t)

let write_leaderboard leaderboard =
  let stats = List.sort compare_scores leaderboard in
  let oc = open_out "leaderboard.txt" in
  print_scores oc stats;
  close_out oc

let draw_lb_text height x1 x2 x3 x4 x5 spacer =
  let push_height = int_of_float (height -. (0.07 *. height)) in
  let push_rank = int_of_float (x1 +. (spacer /. 2.5)) in
  let push_player = int_of_float (x2 +. (spacer /. 2.5)) in
  let push_s_score = int_of_float (x3 +. (spacer /. 3.5)) in
  let push_t_score = int_of_float (x4 +. (spacer /. 3.5)) in
  let push_streak = int_of_float (x5 +. (spacer /. 2.5)) in
  Graphics.moveto push_rank push_height;
  Graphics.draw_string "RANK";
  Graphics.moveto push_player push_height;
  Graphics.draw_string "PLAYER";
  Graphics.moveto push_s_score push_height;
  Graphics.draw_string "SINGLE SCORE";
  Graphics.moveto push_t_score push_height;
  Graphics.draw_string "TOTAL SCORE";
  Graphics.moveto push_streak push_height;
  Graphics.draw_string "STREAK"

let draw_lb_ranks x1 height y_vert spacer =
  let region = height -. (1.5 *. y_vert) in
  let piece = region /. 10.0 in
  let x_push = x1 +. (spacer /. 2.0) in
  let rank_arr =
    [| "10"; "9"; "8"; "7"; "6"; "5"; "4"; "3"; "2"; "1" |]
  in
  for i = 0 to Array.length rank_arr - 1 do
    Graphics.moveto (int_of_float x_push)
      (int_of_float (float_of_int (i + 1) *. piece));
    Graphics.draw_string rank_arr.(i)
  done

let draw_player_stats leaderboard height y_vert spacer x2 x3 x4 x5 =
  let lb_arr = Array.of_list pretty_score_list in
  let piece = (height -. (1.5 *. y_vert)) /. 10.0 in
  for i = 0 to Array.length lb_arr - 1 do
    Graphics.moveto
      (int_of_float (x2 +. (spacer /. 2.0)))
      (int_of_float (float_of_int (i + 1) *. piece));
    Graphics.draw_string lb_arr.(i).username;
    Graphics.moveto
      (int_of_float (x3 +. (spacer /. 2.0)))
      (int_of_float (float_of_int (i + 1) *. piece));
    Graphics.draw_string (string_of_int lb_arr.(i).highest_round_score);
    Graphics.moveto
      (int_of_float (x4 +. (spacer /. 2.0)))
      (int_of_float (float_of_int (i + 1) *. piece));
    Graphics.draw_string (string_of_int lb_arr.(i).max_total_score);
    Graphics.moveto
      (int_of_float (x5 +. (spacer /. 2.0)))
      (int_of_float (float_of_int (i + 1) *. piece));
    Graphics.draw_string (string_of_int lb_arr.(i).max_streak)
  done

(* [leaderboard_math width height] will take in the [width] and [height]
   of the screen to perform mathematical calculations and create values
   for later functions to use in drawing the leaderboard. *)
let leaderboard_math width height =
  let float_w = float_of_int width in
  let float_h = float_of_int height in
  let x_chunk = float_w *. 0.1 in
  let y_chunk = float_h *. 0.1 in
  let y_vert = float_h *. 0.05 in
  let x_spacer = (float_w -. (2.0 *. x_chunk)) /. 5.0 in
  let x2 = x_chunk +. x_spacer in
  let x3 = x2 +. x_spacer in
  let x4 = x3 +. x_spacer in
  let x5 = x4 +. x_spacer in
  let x6 = x5 +. x_spacer in
  let arr1 = [| float_w; float_h; x_chunk; y_chunk; y_vert |] in
  let arr2 = [| x_spacer; x2; x3; x4; x5; x6 |] in
  Array.append arr1 arr2

(* [draw_lb_vertical m] takes a float array [m] from [leaderboard_math]
   and draws the vertical lines for the leaderboard*)
let draw_lb_vertical m =
  Graphics.moveto (int_of_float m.(2)) (int_of_float (m.(1) -. m.(4)));
  Graphics.lineto (int_of_float m.(2)) (int_of_float m.(4));
  Graphics.moveto (int_of_float m.(6)) (int_of_float (m.(1) -. m.(4)));
  Graphics.lineto (int_of_float m.(6)) (int_of_float m.(4));
  Graphics.moveto (int_of_float m.(7)) (int_of_float (m.(1) -. m.(4)));
  Graphics.lineto (int_of_float m.(7)) (int_of_float m.(4));
  Graphics.moveto (int_of_float m.(8)) (int_of_float (m.(1) -. m.(4)));
  Graphics.lineto (int_of_float m.(8)) (int_of_float m.(4));
  Graphics.moveto (int_of_float m.(9)) (int_of_float (m.(1) -. m.(4)));
  Graphics.lineto (int_of_float m.(9)) (int_of_float m.(4));
  Graphics.moveto (int_of_float m.(10)) (int_of_float (m.(1) -. m.(4)));
  Graphics.lineto (int_of_float m.(10)) (int_of_float m.(4))

(* [draw_lb_horizontal m] takes a float array [m] from
   [leaderboard_math] and draws the horizontal lines for the leaderboard*)
let draw_lb_horizontal m =
  Graphics.moveto (int_of_float m.(2)) (int_of_float (m.(1) -. m.(3)));
  Graphics.lineto
    (int_of_float (m.(0) -. m.(2)))
    (int_of_float (m.(1) -. m.(3)))

let draw_lb_lines width height lb =
  let m = leaderboard_math width height in
  draw_lb_horizontal m;
  draw_lb_vertical m;
  draw_lb_text m.(1) m.(2) m.(6) m.(7) m.(8) m.(9) m.(5);
  draw_lb_ranks m.(2) m.(1) m.(3) m.(5);
  draw_player_stats lb m.(1) m.(3) m.(5) m.(6) m.(7) m.(8) m.(9)
