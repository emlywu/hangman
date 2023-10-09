(** The graphics and logic associated with the game's menu. *)

open Graphics

(** The type representing a color palette for the game. *)
type palette

(** The standard color palette. *)
val standard_palette : palette

(** The Cornell color scheme palette. *)
val cornell_palette : palette

(** The sunset park color scheme palette. *)
val sunset_park : palette

(** The Ithaca is gorges color scheme palette. *)
val ithaca_is_gorges : palette

(** The Cayuga's waters color scheme palette. *)
val cayugas_waters : palette

(** [background_color p] is the background color given by [p]. *)
val background_color : palette -> color

(** [background_light_color p] is the light color for the buttons given
    by [p]. *)
val button_light_color : palette -> color

(** [background_med_color p] is the medium color for the buttons given
    by [p]. *)
val button_med_color : palette -> color

(** [background_dark_color p] is the dark color for the buttons given by
    [p]. *)
val button_dark_color : palette -> color

(** [text_color p] is the text color given by [p]. *)
val text_color : palette -> color

(** [drawing_color p] is the drawing color given by [p]. *)
val drawing_color : palette -> color

(** [make_palette] draws the palette choices. *)
val make_palette : unit -> unit

(** [make_instructions] draws the instructions. *)
val make_instructions : unit -> unit

(** [exit_window_button x y] draws an exit button with the lower left
    corner at ([x], [y]). *)
val exit_window_button : int -> int -> unit

(** [back_button x y] draws a button to return to the menu selection
    screen with the lower left corner of the button at ([x], [y])*)
val back_button : int -> int -> unit

(** [choose_palette] waits for the user to click on a palette choice and
    returns the palette they chose. *)
val choose_palette : unit -> palette

(* (** [draw_menu_button x y w h] draws a gray menu button at ([x], [y])
   of width [w] and height [h]. *) val draw_menu_button : int -> int ->
   int -> int -> unit *)

(** [show_menu] draws a menu and shows 4 buttons for the 4 different
    menu options. *)
val show_menu : unit -> unit

(** [find_index c lst acc] finds the index of char [c] in the list of
    chars for the row that [c] is in. Note: this function was added to
    the public documentation so that it could be referenced in our
    testing suite. *)
val find_index : 'a -> 'a list -> int -> int

(** [show_keyboard b x y] displays a QWERTY keyboard on the screen
    starting at ([x], [y]) if [b] is true. [b] is true if the "show
    keyboard" button in the menu has been clicked *)
val show_keyboard : bool -> int -> int -> Graphics.color -> unit

(** [detect_letter x y] is the character that the user's mouse is
    hovering over. ([x], [y]) is the bottom left corner of the keyboard *)
val detect_letter : int -> int -> char
