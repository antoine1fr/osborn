open Std

type post = {
  client_path : string;
} [@@deriving yojson]

type scope = {
  conf : Conf.t;
  posts : post list;
} [@@deriving yojson]

let process_index working_dir build_dir scope =
  Utils.read_file (working_dir ^ "/index.html") >>= fun index_tpl_str ->
  let index_tpl = Mustache.of_string index_tpl_str in
  let index_html = Mustache.render index_tpl scope in
  Utils.write_file (build_dir ^ "/index.html") index_html

let get_posts working_dir =
  let post_dir = working_dir ^ "/_posts" in
  Utils.get_folder_content post_dir >>= fun post_list ->
  post_list
    |> List.filter (fun filename -> filename <> "." && filename <> "..")
    |> List.filter (fun filename -> Utils.file_extension filename = "md")
    |> List.map (fun filename -> post_dir ^ "/" ^ filename)
    |> List.map (fun client_path -> {client_path})
    |> Result.return

let build working_dir =
  let build_dir = working_dir ^ "/_build" in
  get_posts working_dir >>= fun posts ->
  let conf = working_dir ^ "/config.sexp"
    |> Conf.from_file
    |> Result.get_ok ~default:Conf.default in
  let scope = {conf; posts} |> scope_to_yojson |> Utils.ezjsonm_of_yojson in
  process_index working_dir build_dir scope
