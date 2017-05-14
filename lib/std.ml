include Sexplib.Std
include (Basics: module type of Basics with module Map := OBMap)
include Result.Infix

module Map = struct
  include OBMap

  let of_key_value_list (type a) (l: (string * a) list): a t =
    List.fold_left (fun accu (key, value) ->
      add key value accu) empty l

  let sexp_of_t sexp_of_value map =
    let open Sexplib.Type in
    let key_values =
      fold (fun key value accu ->
        List [Atom key; sexp_of_value value] :: accu) map [] in
    List key_values

  let key_value_of_sexp sexp =
    let open Sexplib.Type in
    match sexp with
    | List [Atom key; value] -> (key, value)
    | _ ->
      invalid_arg "Std.Map.key_value_of_sexp: expected List of key-values"

  let t_of_sexp value_of_sexp sexp =
    let open Sexplib.Type in
    match sexp with
    | List key_values ->
      List.fold_left (fun accu sexp ->
        let (key, value) = key_value_of_sexp sexp in
        add key (value_of_sexp value) accu) empty key_values
    | Atom _ ->
      invalid_arg "Std.Map.t_of_sexp: expected List of key-values"
end
