open Std

type post = {
  client_path : string;
  source_path : string;
  build_path : string;
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
    |> List.map (fun filename ->
      let base_name = Utils.base_filename filename in
      let client_path = "_posts/" ^ base_name ^ ".html" in
      let build_path = working_dir ^  "/_build/_posts/" ^ base_name ^ ".html" in
      let source_path = working_dir ^ "/_posts/" ^ base_name ^ ".md" in
      {client_path; source_path; build_path})
    |> Result.return

let mustache_filter scope str =
  let mustache = Mustache.of_string str in
  Mustache.render mustache scope

let markdown_filter str =
  let markdown = Omd.of_string str in
  Omd.to_html ~pindent:true markdown

let process_post working_dir build_dir scope post =
  Utils.read_file post.source_path >>= fun content ->
  let html = content
    |> mustache_filter scope
    |> markdown_filter in
  Utils.write_file post.build_path html

let process_posts working_dir build_dir scope posts =
  Result.traverse (process_post working_dir build_dir scope) posts

let build working_dir =
  let build_dir = working_dir ^ "/_build" in
  get_posts working_dir >>= fun posts ->
  let conf = working_dir ^ "/config.sexp"
    |> Conf.from_file
    |> Result.get_ok ~default:Conf.default in
  let scope = {conf; posts} |> scope_to_yojson |> Utils.ezjsonm_of_yojson in
  process_index working_dir build_dir scope >>= fun () ->
  process_posts working_dir build_dir scope posts >>= fun _ ->
  Result.return ()
