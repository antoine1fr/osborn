open Std
open Parser.Infix

type t = string Map.t [@@ deriving sexp]

let symbol_char = Parser.not_chars ":\n\r"

let symbol =
  Parser.plus symbol_char >>= fun chars ->
  Parser.string_of_char_list chars >>= fun symbol ->
  Parser.return symbol

let value_char = Parser.not_chars "\n\r"

let value =
  Parser.star value_char >>= fun chars ->
  Parser.string_of_char_list chars

let key_value =
  symbol >>= fun key ->
  Parser.specific_char ':' >>= fun _ ->
  value >>= fun value ->
  Parser.opt Parser.eol >>= fun _ ->
  Parser.return (key, String.trim value)

let front_matter: t Parser.t =
  Parser.string "---" >>= fun _ ->
  Parser.eol >>= fun _ ->
  Parser.star key_value >>= fun key_values ->
  Parser.string "---" >>= fun _ ->
  let front_matter = List.fold_left (fun map (key, value) ->
    Map.add key value map) Map.empty key_values in
  Parser.return front_matter

let front_matter' = Parser.opt front_matter
