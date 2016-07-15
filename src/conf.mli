type t = {
  blog_name : string;
  author_name : string;
  author_email : string;
  author_twitter : string;
} [@@deriving sexp]

val default : t
val from_file : string -> (t, string) Result.result
