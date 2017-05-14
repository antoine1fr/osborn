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

exception Partial_not_found of string

let mustache_filter partial scope str =
  try
    let mustache = str
      |> Mustache.of_string
      |> Mustache.expand_partials (fun name ->
        match name with
        | "post_content" -> Some partial
        | _ -> raise (Partial_not_found name)) in
    Ok (Mustache.render mustache scope)
  with exn ->
    let msg = Printexc.to_string exn in
    Error msg

let markdown_filter str =
  let markdown = Omd.of_string str in
  Ok (Omd.to_html ~pindent:true markdown)

let process_post working_dir build_dir conf layout post =
  Utils.read_file post.source_path >>= fun partial_str ->
  let post_json = post |> post_to_yojson |> Utils.ezjsonm_value_of_yojson in
  let scope = ("post", post_json) :: Conf.to_scope conf in
  markdown_filter partial_str >>= fun mustache_str ->
  let partial = Mustache.of_string mustache_str in
  mustache_filter partial (`O scope) layout >>= fun html_str ->
  Utils.write_file post.build_path html_str

let process_posts working_dir build_dir conf posts =
  Utils.read_file (working_dir ^ "/post-layout.html") >>= fun layout ->
  Result.traverse (process_post working_dir build_dir conf layout) posts

let build working_dir =
  let build_dir = working_dir ^ "/_build" in
  get_posts working_dir >>= fun posts ->
  let conf = working_dir ^ "/config.sexp"
    |> Conf.from_file
    |> Result.get_ok ~default:Conf.default in
  let scope = {conf; posts} |> scope_to_yojson |> Utils.ezjsonm_of_yojson in
  process_index working_dir build_dir scope >>= fun () ->
  process_posts working_dir build_dir conf posts >>= fun _ ->
  Result.return ()
