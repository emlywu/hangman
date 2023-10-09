(** The lists of words for each category. *)

open Yojson.Basic.Util

(** The Cornell category JSON. *)
val cornell : Yojson.Basic.t

(** The countries category JSON. *)
val countries : Yojson.Basic.t

(** The crayon colors category JSON. *)
val crayon_colors : Yojson.Basic.t

(** The Disney characters category JSON. *)
val disney_characters : Yojson.Basic.t

(** The Harry Potter category JSON. *)
val harry_potter : Yojson.Basic.t

(** The zoo animals category JSON. *)
val zoo_animals : Yojson.Basic.t

(** The OCaml category JSON. *)
val ocaml : Yojson.Basic.t

(** [get_word_list json] is a string array representing the words stored
    in [json]. *)
val get_word_list : Yojson.Basic.t -> string array
