open Std

let rec ezjsonm_value_of_yojson = function
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

let ezjsonm_of_yojson yojson =
  match ezjsonm_value_of_yojson yojson with
  | `A x -> `A x
  | `O x -> `O x
  | x -> `A [x]

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

let get_folder_content path =
  let handle_ref = ref None in
  try
    let handle = Unix.opendir path in
    let rec loop accu =
      try
        let entry = Unix.readdir handle in
        loop (entry :: accu)
      with End_of_file -> accu
    in
    let entries = loop [] in
    Unix.closedir handle;
    Ok entries
  with exn ->
    (match !handle_ref with
    | Some handle -> Unix.closedir handle
    | None -> ());
    let msg = Printexc.to_string exn in
    Error msg

let file_extension filename =
  let regexp = Str.regexp "\\(\\.\\(.*\\)\\)?$" in
  let _ = Str.search_forward regexp filename 0 in
  Str.matched_group 2 filename

let base_filename filename =
  let regexp = Str.regexp "\\(\\.\\(.*\\)\\)?$" in
  let i = Str.search_forward regexp filename 0 in
  Str.string_before filename i
