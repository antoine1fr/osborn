open Std

module Cli = struct
  open Cmdliner

  let working_dir =
    let doc = "Directory in which reside the sources of the  blog." in
    Arg.(
      value
      & opt dir "./"
      & info ["d"; "working-dir"] ~doc)

  let build = Term.(const Build.build $ working_dir)

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
