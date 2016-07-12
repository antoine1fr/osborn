let launch working_dir = ()

module Cli = struct
  open Cmdliner

  let working_dir =
    let doc = "Directory in which to create the blog." in
    Arg.(
      value
      & opt dir "./"
      & info ["d"; "working-dir"] ~doc)

  let init = Term.(const launch $ working_dir)

  let init_info =
    let doc = "Simple static blog generator written in OCaml." in
    let man = [
      `S "BUGS";
      `P "Email bug reports to <antoine1fr at
      gmail dot com>."] in
    Term.info "launch" ~doc ~man

  let info =
    let doc = "Simple static blog generator written in OCaml." in
    let man = [
      `S "BUGS";
      `P "Email bug reports to <antoine1fr at
      gmail dot com>."] in
    Term.info "launch" ~doc ~man
end

let () =
  let launch = (Cli.init, Cli.info) in
  let commands = [launch] in
  match Cmdliner.Term.eval_choice launch commands with
  | `Error msg -> exit 1
  | _ -> exit 0
