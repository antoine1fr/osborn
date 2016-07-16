type t = {
  blog_name : string;
  author_name : string;
  author_email : string;
  author_twitter : string;
} [@@deriving sexp, yojson]

val default : t
val from_file : string -> (t, string) Result.result
