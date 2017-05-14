open Std

type state = string * int

and 'a t = state -> state * ('a, string) Result.t

module ParserKernel : Monad.Kernel1
 with type 'a t = 'a t

module Applicative: Applicative.S1 with type 'a t := 'a t
include module type of Applicative.Core

module Monad: Monad.S1 with type 'a t := 'a t
include module type of Monad.Core

module Infix: sig
  include module type of Applicative.Infix
  include module type of Monad.Infix
end

val char : char t
val choose : 'a t list -> 'a t
val count : int -> 'a t -> 'a list t
val eol: string t
val eol_char: char t
val error: string -> _ t
val next : char t
val not_char : char -> char t
val not_chars : string -> char t
val opt : 'a t -> 'a option t
val plus : 'a t -> 'a list t
val specific_char : char -> char t
val specific_chars : string -> char t
val star : 'a t -> 'a list t
val string : string -> string t
val string_of_char_list : char list -> string t
