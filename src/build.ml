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
