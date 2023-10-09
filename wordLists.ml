open Yojson.Basic.Util

let cornell = Yojson.Basic.from_file "cornell.json"

let countries = Yojson.Basic.from_file "countries.json"

let crayon_colors = Yojson.Basic.from_file "crayon_colors.json"

let disney_characters = Yojson.Basic.from_file "disney_characters.json"

let harry_potter = Yojson.Basic.from_file "harry_potter.json"

let zoo_animals = Yojson.Basic.from_file "zoo_animals.json"

let ocaml = Yojson.Basic.from_file "ocaml.json"

let get_word_list json =
  json |> to_assoc |> List.assoc "words" |> Yojson.Basic.Util.to_list
  |> List.map Yojson.Basic.Util.to_string
  |> Array.of_list
