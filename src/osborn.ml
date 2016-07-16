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

type scope = {
  conf : Conf.t;
} [@@deriving yojson]

let read_file path =
  let ic_ref = ref None in
  try
    let ic = open_in path in
    ic_ref := Some ic;
    let len = in_channel_length ic in
    let str = String.make len ' ' in
    let _ = input ic str 0 len in
    close_in ic;
    Ok str
  with exn ->
    (match !ic_ref with
    | Some ic -> close_in ic
    | None -> ());
    let msg = Printexc.to_string exn in
    Error msg

let write_file path content =
  let oc_ref = ref None in
  try
    let oc = open_out path in
    oc_ref := Some oc;
    output_string oc content;
    close_out oc;
    Ok ()
  with exn ->
    (match !oc_ref with
    | Some oc -> close_out oc
    | None -> ());
    let msg = Printexc.to_string exn in
    Error msg

let build working_dir =
  let build_dir = working_dir ^ "/_build" in
  let conf = working_dir ^ "/config.sexp"
    |> Conf.from_file
    |> Result.get_ok ~default:Conf.default in
  let scope = {conf} |> scope_to_yojson |> ezjsonm_of_yojson in
  read_file (working_dir ^ "/index.html") >>= fun index_tpl_str ->
  let index_tpl = Mustache.of_string index_tpl_str in
  let index_html = Mustache.render index_tpl scope in
  write_file (build_dir ^ "/index.html") index_html

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
  | `Ok (Error msg) ->
    Printf.fprintf stderr "Fatal error: %s\n" msg;
    exit 1
  | `Ok (Ok ()) | `Help | `Version -> exit 0
