open Std

let rec ezjsonm_value_of_yojson : Yojson.Safe.json -> Mustache.Json.value = function
  | `Assoc x ->
    let x' = List.map (fun (key, value) ->
      (key, ezjsonm_value_of_yojson value)) x in
    `O x'
  | `Bool x -> `Bool x
  | `Float x -> `Float x
  | `Int x -> `Float (float_of_int x)
  | `Intlit x -> `String x
  | `Null -> `Null
  | `String x -> `String x
  | `Tuple x | `List x -> `A (List.map ezjsonm_value_of_yojson x)
  | `Variant _ -> `Null

let ezjsonm_of_yojson yojson : Mustache.Json.t =
  match ezjsonm_value_of_yojson yojson with
  | `A x -> `A x
  | `O x -> `O x
  | x -> `A [x]

let build working_dir =
  let config = working_dir ^ "/config.sexp"
    |> Conf.from_file
    |> Result.get_ok ~default:Conf.default in
  ()

module Cli = struct
  open Cmdliner

  let working_dir =
    let doc = "Directory in which reside the sources of the  blog." in
    Arg.(
      value
      & opt dir "./"
      & info ["d"; "working-dir"] ~doc)

  let build = Term.(const build $ working_dir)

  let info =
    let doc = "Simple static blog generator written in OCaml." in
    let man = [
      `S "BUGS";
      `P "Email bug reports to <antoine1fr at gmail dot com>."] in
    Term.info "Osborn" ~doc ~man
end

let () =
  let launch = (Cli.build, Cli.info) in
  match Cmdliner.Term.eval launch with
  | `Error msg -> exit 1
  | _ -> exit 0
