open OUnit2
open Std

type result = (FrontMatter.t, string) Result.t [@@deriving sexp]

let string_of_result result =
  result
  |> sexp_of_result
  |> Sexplib.Sexp.to_string_hum

let front_matter_string = {ft|---
Title: Lorem ipsum
Tags: foo bar baz
---|ft}

let suite = "FrontMatter" >::: [
  "it should parse front matters" >:: begin fun _ ->
    let state = (front_matter_string, 0) in
    let (_, front_matter) = FrontMatter.front_matter state in
    assert_equal ~printer:string_of_result
      (Ok (Map.of_key_value_list [
        ("Title", "Lorem ipsum");
        ("Tags", "foo bar baz")]))
      front_matter
  end;
]
