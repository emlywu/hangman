open Graphics

let draw_banner_win x y w h col =
  Graphics.set_color col;
  Graphics.draw_rect x y w h;
  Graphics.fill_rect x y w h;
  let left_banner_1 = [| (450, 368); (440, 368); (445, 381) |] in
  let left_banner_2 = [| (440, 394); (450, 394); (450, 368) |] in
  let left_banner = Array.append left_banner_1 left_banner_2 in
  let right_banner_1 = [| (594, 368); (604, 368); (599, 381) |] in
  let right_banner_2 = [| (604, 394); (594, 394); (594, 368) |] in
  let right_banner = Array.append right_banner_1 right_banner_2 in
  Graphics.draw_poly left_banner;
  Graphics.fill_poly left_banner;
  Graphics.draw_poly right_banner;
  Graphics.fill_poly right_banner

let draw_banner_lose x y w h col =
  Graphics.set_color col;
  Graphics.draw_rect x y w h;
  Graphics.fill_rect x y w h;
  let left_banner_1 = [| (450, 368); (440, 368); (445, 381) |] in
  let left_banner_2 = [| (440, 394); (450, 394); (450, 368) |] in
  let left_banner = Array.append left_banner_1 left_banner_2 in
  let right_banner_1 = [| (594, 368); (604, 368); (599, 381) |] in
  let right_banner_2 = [| (604, 394); (594, 394); (594, 368) |] in
  let right_banner = Array.append right_banner_1 right_banner_2 in
  Graphics.draw_poly left_banner;
  Graphics.fill_poly left_banner;
  Graphics.draw_poly right_banner;
  Graphics.fill_poly right_banner

(* gallows takes in top corner coords, and length for each piece of wood*)
let draw_gallows x0 y0 len1 len2 len3 len4 len5 =
  let y1 = y0 - len1 in
  let x2 = x0 + len2 in
  let y3 = y0 - len3 in
  let x4 = x0 - len4 in
  let x5 = x4 + len4 + len5 in
  Graphics.moveto x0 y0;
  Graphics.lineto x0 y1;
  Graphics.moveto x0 y0;
  Graphics.lineto x2 y0;
  Graphics.moveto x2 y0;
  Graphics.lineto x2 y3;
  Graphics.moveto x4 y1;
  Graphics.lineto x5 y1

(* draw_body will draw a line of length n up from coords given*)
let draw_body x0 y0 len =
  let y1 = y0 + len in
  Graphics.moveto x0 y0;
  Graphics.lineto x0 y1

(* draw_head will draw a circle with center coords and radius r*)
let draw_head x0 y0 r = draw_circle x0 y0 r

let draw_l_limb x0 y0 len =
  let new_len = float_of_int len in
  let c = new_len in
  let b = new_len -. (0.3333333 *. new_len) in
  let num_to_floor = sqrt ((c *. c) -. (b *. b)) in
  let a = int_of_float (Float.floor num_to_floor) in
  let x1 = x0 - a in
  let y1 = y0 - int_of_float b in
  Graphics.moveto x0 y0;
  Graphics.lineto x1 y1

let draw_r_limb x0 y0 len =
  let new_len = float_of_int len in
  let c = new_len in
  let b = new_len -. (0.333333333 *. new_len) in
  let num_to_floor = sqrt ((c *. c) -. (b *. b)) in
  let a = int_of_float (Float.floor num_to_floor) in
  let x1 = x0 + a in
  let y1 = y0 - int_of_float b in
  Graphics.moveto x0 y0;
  Graphics.lineto x1 y1

let draw_hat x0 y0 r =
  let height = int_of_float (2.0 *. float_of_int r *. 0.33333333) in
  let push = int_of_float (0.875 *. float_of_int r) in
  let x1 =
    int_of_float (float_of_int x0 -. (0.25 *. 2.0 *. float_of_int r))
  in
  let x2 =
    int_of_float (float_of_int x0 +. (0.25 *. 2.0 *. float_of_int r))
  in
  Graphics.moveto x0 y0;
  Graphics.lineto (x0 - push) y0;
  Graphics.moveto x0 y0;
  Graphics.lineto (x0 + push) y0;
  Graphics.moveto x1 y0;
  Graphics.lineto x1 (y0 + height);
  Graphics.moveto x2 y0;
  Graphics.lineto x2 (y0 + height);
  Graphics.moveto x1 (y0 + height);
  Graphics.lineto x2 (y0 + height)

let find_point x0 y0 len left_or_right =
  let new_len = float_of_int len in
  let c = new_len in
  let b = new_len -. (0.3333333 *. new_len) in
  let num_to_floor = sqrt ((c *. c) -. (b *. b)) in
  let a = int_of_float (Float.floor num_to_floor) in
  let x1 = if left_or_right = "left" then x0 - a else x0 + a in
  (* if (left_or_right = "left") then let x1 = x0 - a else let x1 = x0 +
     a in *)
  let y1 = y0 - int_of_float b in
  (x1, y1)

let draw_foot_l x0 y0 limb_len =
  let foot_len = int_of_float (float_of_int limb_len *. 0.333333) in
  Graphics.moveto x0 y0;
  Graphics.lineto (x0 - foot_len) y0

let draw_foot_r x0 y0 limb_len =
  let foot_len = int_of_float (float_of_int limb_len *. 0.333333) in
  Graphics.moveto x0 y0;
  Graphics.lineto (x0 + foot_len) y0

let draw_dead_smile x0 y0 r =
  let pull = int_of_float (0.25 *. float_of_int (2 * r)) in
  let smile = r / 2 in
  draw_arc x0 (y0 - pull) smile smile 0 180

let draw_left_eye x0 y0 r =
  let d = 2 * r in
  let x1 = x0 - int_of_float (0.2 *. float_of_int d) in
  let y1 = y0 + int_of_float (0.2 *. float_of_int d) in
  let x_left_up1 = x1 - int_of_float (0.08 *. float_of_int d) in
  let y_left_up1 = y1 + int_of_float (0.08 *. float_of_int d) in
  let x_right_up1 = x1 + int_of_float (0.08 *. float_of_int d) in
  let y_right_up1 = y1 + int_of_float (0.08 *. float_of_int d) in
  let x_left_down1 = x1 - int_of_float (0.08 *. float_of_int d) in
  let y_left_down1 = y1 - int_of_float (0.08 *. float_of_int d) in
  let x_right_down1 = x1 + int_of_float (0.08 *. float_of_int d) in
  let y_right_down1 = y1 - int_of_float (0.08 *. float_of_int d) in
  Graphics.moveto x1 y1;
  Graphics.lineto x_left_up1 y_left_up1;
  Graphics.moveto x1 y1;
  Graphics.lineto x_right_up1 y_right_up1;
  Graphics.moveto x1 y1;
  Graphics.lineto x_left_down1 y_left_down1;
  Graphics.moveto x1 y1;
  Graphics.lineto x_right_down1 y_right_down1

let draw_right_eye x0 y0 r =
  let d = 2 * r in
  let x2 = x0 + int_of_float (0.2 *. float_of_int d) in
  let y2 = y0 + int_of_float (0.2 *. float_of_int d) in
  let x_left_up2 = x2 - int_of_float (0.08 *. float_of_int d) in
  let y_left_up2 = y2 + int_of_float (0.08 *. float_of_int d) in
  let x_right_up2 = x2 + int_of_float (0.08 *. float_of_int d) in
  let y_right_up2 = y2 + int_of_float (0.08 *. float_of_int d) in
  let x_left_down2 = x2 - int_of_float (0.08 *. float_of_int d) in
  let y_left_down2 = y2 - int_of_float (0.08 *. float_of_int d) in
  let x_right_down2 = x2 + int_of_float (0.08 *. float_of_int d) in
  let y_right_down2 = y2 - int_of_float (0.08 *. float_of_int d) in
  Graphics.moveto x2 y2;
  Graphics.lineto x_left_up2 y_left_up2;
  Graphics.moveto x2 y2;
  Graphics.lineto x_right_up2 y_right_up2;
  Graphics.moveto x2 y2;
  Graphics.lineto x_left_down2 y_left_down2;
  Graphics.moveto x2 y2;
  Graphics.lineto x_right_down2 y_right_down2

let draw_secret_man placeholder =
  draw_gallows placeholder 650 125 55 18 30 75;
  draw_head 545 617 15;
  draw_body 545 557 45;
  draw_l_limb 545 591 15;
  draw_r_limb 545 591 15;
  draw_l_limb 545 557 15;
  draw_r_limb 545 557 15;
  draw_left_eye 545 617 15;
  draw_right_eye 545 617 15;
  draw_dead_smile 545 617 15

(* [outline x y w h a b] outlines the button *)
let outline x y w h a b =
  set_color black;
  moveto x y;
  lineto (x - a) (y - b);
  lineto (x + w + a) (y - b);
  lineto (x + w) y;
  lineto x y;
  lineto x (y + h);
  lineto (x - a) (y + h + b);
  lineto (x - a) (y - b);
  moveto x (y + h);
  lineto (x + w) (y + h);
  lineto (x + w + a) (y + h + b);
  lineto (x - a) (y + h + b);
  moveto (x + w) (y + h);
  lineto (x + w) y;
  lineto (x + w + a) (y - b);
  lineto (x + w + a) (y + h + b)

(* [draw_button_light] draws the part of the button to be colored in
   with the light color*)
let draw_button_light light x y w h a b =
  set_color light;
  fill_poly [| (x, y); (x - a, y - b); (x - a, y + h + b); (x, y + h) |];
  fill_poly
    [|
      (x - a, y + h + b);
      (x, y + h);
      (x + w, y + h);
      (x + w + a, y + h + b);
    |]

(* [draw_button_dark] draws the part of the button to be colored in with
   the dark color*)
let draw_button_dark dark x y w h a b =
  set_color dark;
  fill_poly [| (x - a, y - b); (x, y); (x + w, y); (x + w + a, y - b) |];
  fill_poly
    [|
      (x + w, y);
      (x + w + a, y - b);
      (x + w + a, y + h + b);
      (x + w, y + h);
    |]

(* [draw_button_medium] draws the part of the button to be colored in
   with the medium color*)
let draw_button_medium med x y w h a b =
  set_color med;
  fill_rect x y w h

let draw_button light med dark x y w h a b text =
  draw_button_light light x y w h a b;
  draw_button_dark dark x y w h a b;
  draw_button_medium med x y w h a b;
  outline x y w h a b;
  set_color black;
  moveto (x + (w / 2) - (String.length text * 3)) (y + (h / 2) - 3);
  draw_string text

let mouse_in_button x y w h =
  let mouse_x = fst (mouse_pos ()) in
  let mouse_y = snd (mouse_pos ()) in
  (mouse_x >= x && mouse_x <= x + w) && mouse_y >= y && mouse_y <= y + h
