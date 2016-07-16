open Std

type scope = {
  conf : Conf.t;
} [@@deriving yojson]

let build working_dir =
  let build_dir = working_dir ^ "/_build" in
  let conf = working_dir ^ "/config.sexp"
    |> Conf.from_file
    |> Result.get_ok ~default:Conf.default in
  let scope = {conf} |> scope_to_yojson |> Utils.ezjsonm_of_yojson in
  Utils.read_file (working_dir ^ "/index.html") >>= fun index_tpl_str ->
  let index_tpl = Mustache.of_string index_tpl_str in
  let index_html = Mustache.render index_tpl scope in
  Utils.write_file (build_dir ^ "/index.html") index_html

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
