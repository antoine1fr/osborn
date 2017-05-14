open Std

type state = string * int [@@deriving sexp]

and 'a t = state -> state * ('a, string) Result.t

module ParserKernel = struct
  type nonrec 'a t = 'a t

  let return x = fun state -> (state, Ok x)

  let bind parser f = fun state ->
    match parser state with
    | (state', Ok x) ->
        let parser' = f x in
        parser' state'
    | (_, Error msg) -> (state, Error msg)
end

module Monad = Monad.Make1 (ParserKernel)
include Monad.Core

module Applicative = Applicative.Make1 (ParserKernel)
include Applicative.Core

module Infix = struct
  include Monad.Infix
  include Applicative.Infix
end

open Infix

let string_of_char_list chars =
  let buffer = Buffer.create 10 in
  List.iter (Buffer.add_char buffer) chars;
  return (Buffer.contents buffer)

let rec choose parsers = fun state ->
  match parsers with
  | [] -> (state, Error "Parser.choose")
  | head :: tail ->
    match head state with
    | (_, Ok _) as ok -> ok
    | (_, Error _) -> choose tail state

let error msg = fun state -> (state, Result.error msg)

let next = fun state ->
  let (str, i) = state in
  if (i < String.length str) then
    ((str, i + 1), Ok str.[i])
  else (state, Result.error "end of file")

let char = next

let opt parser = fun state ->
  match parser state with
  | (state', Ok x) -> (state', Ok (Some x))
  | (_, Error _) -> (state, Ok None)

let star parser = fun state ->
  let rec loop state accu =
    match parser state with
    | (state', Ok x) -> loop state' (x :: accu)
    | (_, Error _) -> (state, Ok (List.rev accu))
  in loop state []

let plus parser =
  parser >>= fun x ->
  star parser >>= fun accu ->
  return (x :: accu)

let not_char c1 =
  char >>= fun c2 ->
  if c1 <> c2 then
    return c2
  else error (Printf.sprintf "didn't expect char '%c'" c1)

let not_chars chars =
  char >>= fun c ->
  match String.contains chars c with
  | true -> error (Printf.sprintf "didn't expect char '%c'" c)
  | false -> return c

let specific_char c1 =
  char >>= fun c2 ->
  if c1 = c2 then
    return c1
  else error (Printf.sprintf "expected '%c'" c1)

let specific_chars chars =
  char >>= fun c ->
  match String.contains chars c with
  | false -> error (Printf.sprintf "didn't expect char '%c'" c)
  | true -> return c

let eol_char =
  char >>= fun c ->
  match c with
  | '\r' | '\n' -> return c
  | _ -> error (Printf.sprintf "expected EOL character but got '%c'" c)

let eol =
  plus (specific_chars "\n\r") >>= fun chars ->
  string_of_char_list chars

let count n parser =
  let rec count accu n parser = fun state ->
    if n < 1 then
      (state, Result.return (List.rev accu))
    else
      match parser state with
      | (state', Ok x) -> count (x :: accu) (n - 1) parser state'
      | (_, Error _) as result -> result in
  count [] n parser

let string str =
  let length = String.length str in
  count length next >>= fun chars ->
  string_of_char_list chars
