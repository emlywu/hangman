(** The functions that primarily draw elements on the screen. *)

(** [draw_banner_win x y w h] draws a rectangular banner of width [w]
    and height [h] with the bottom left corner at ([x], [y]). *)
val draw_banner_win : int -> int -> int -> int -> Graphics.color -> unit

(** [draw_banner_lose x y w h] draws a rectangular banner of width [w]
    and height [h] with the bottom left corner at (x, y). *)
val draw_banner_lose :
  int -> int -> int -> int -> Graphics.color -> unit

(** [draw_gallows x0 y0 len1 len2 len3 len4 len5] takes in a pair of
    ([x0],[y0]) starting coordinates as the upper left corner of the
    gallows, and length for each piece of wood. [len1] indicates the
    longest length of wood, [len2] indicates the length of wood that
    overhangs horizontally, [len3] indicates the length of wood that
    attaches to the top of the hanged man, [len4] indicates the left
    piece of the floor piece of wood, and [len5] indicates the right
    piece of the floor piece of wood. *)
val draw_gallows : int -> int -> int -> int -> int -> int -> int -> unit

(** [draw_body x0 y0 len] takes in a pair of ([x0],[y0]) starting
    coordinates and draws a line of length [len] vertically up from the
    starting coordinates.*)
val draw_body : int -> int -> int -> unit

(** [draw_head x0 y0 r] takes in a pair of ([x0],[y0]) starting
    coordinates as the center of the circle, and draws a circle with
    radius [r]*)
val draw_head : int -> int -> int -> unit

(** [draw_l_limb x0 y0 len] takes in a pair of ([x0],[y0]) starting
    coordinates and draws a line of length [len] to the left at an angle
    from the starting coordinates.*)
val draw_l_limb : int -> int -> int -> unit

(** [draw_r_limb x0 y0 len] takes in a pair of ([x0],[y0]) starting
    coordinates and draws a line of length [len] to the right at an
    angle from the starting coordinates.*)
val draw_r_limb : int -> int -> int -> unit

(** [find_point x0 y0 len left_or_right] takes in the same pair of
    ([x0],[y0]) starting coordinates and length [len] from the
    draw_l_limb and draw_r_limb functions. It also takes in a string
    [left_or_right] indicating whether the point needed to be found is
    from the right or left limb. It then tells returns a tuple
    indicating the (x,y) coordinates locating the end of the limb.*)
val find_point : int -> int -> int -> string -> int * int

(** [draw_hat x0 y0 r] takes in a pair of ([x0],[y0]) starting
    coordinates that are at the top center of the head and [r] which is
    the radius of the head and draws a hat right above the head. *)
val draw_hat : int -> int -> int -> unit

(** [draw_foot_l x0 y0 limb_len] takes in a pair of ([x0],[y0]) starting
    coordinates that are at the end of the left limb and draws a
    horizontal line using [limb_len], the length of the left leg limb. *)
val draw_foot_l : int -> int -> int -> unit

(** [draw_foot_r x0 y0 limb_len] takes in a pair of ([x0],[y0]) starting
    coordinates that are at the end of the right limb and draws a
    horizontal line using [limb_len], the length of the right leg limb. *)
val draw_foot_r : int -> int -> int -> unit

(** [draw_dead_smile x0 y0 r] takes in a pair of ([x0],[y0]) starting
    coordinates as the center of the head and [r] as the radius of the
    head, and draws an arc to represent a dead smile. *)
val draw_dead_smile : int -> int -> int -> unit

(** [draw_left_eye x0 y0 r] takes in a pair of ([x0],[y0]) starting
    coordinates as the center of the head and [r] as the radius of the
    head, and draws a cross to represent a left dead eye. *)
val draw_left_eye : int -> int -> int -> unit

(** [draw_right_eye x0 y0 r] takes in a pair of ([x0],[y0]) starting
    coordinates as the center of the head and [r] as the radius of the
    head, and draws a cross to represent a left dead eye. *)
val draw_right_eye : int -> int -> int -> unit

(** [draw_secret_man placeholder] takes in [placeholder] as an
    x-coordinate and draws a small hangman on the screen. *)
val draw_secret_man : int -> unit

(** [draw_button light med dark x y w h a b text] draws a rectangular
    button of width [w] and height [h] with the bottom left corner at
    (x, y) and string [text] in the center. [a] represents how wide, and
    [b] represents how tall the trapezoids that make up the reliefs of
    the button are. *)
val draw_button :
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  string ->
  unit

(** [mouse_in_button x y w h] returns true if the mouse is hovering over
    a button of width [w] and height [h] with the bottom left corner at
    ([x], [y]) *)
val mouse_in_button : int -> int -> int -> int -> bool
