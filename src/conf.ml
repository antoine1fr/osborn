open Std

type t = {
  blog_name : string;
  author_name : string;
  author_email : string;
  author_twitter : string;
} [@@deriving sexp, yojson]

let default = {
  blog_name = "My awesome blog";
  author_name = "John Doe";
  author_email = "john@doe.net";
  author_twitter = "johndoe";
}

let from_file path =
  try
    Utils.read_file path >>= fun str ->
    Ok (str |> String.trim |> Sexplib.Sexp.of_string |> t_of_sexp)
  with exn ->
    let msg = Printexc.to_string exn in
    Error msg
