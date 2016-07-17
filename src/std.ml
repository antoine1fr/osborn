module Result_ = Result

module Result : sig
  type ('ok, 'err) t = ('ok, 'err) Result_.result =
    | Ok of 'ok
    | Error of 'err

  val get_ok : ?default : 'ok -> ('ok, _) t -> 'ok
  val get_error : (_, 'err) t -> 'err
  val return : 'ok -> ('ok, _) t
  val ok : 'ok -> ('ok, _) t
  val error : 'err -> (_, 'err) t
  val bind : ('a, 'err) t -> ('a -> ('b, 'err) t) -> ('b, 'err) t
  val map : ('a -> 'b) -> ('a, 'err) t -> ('b, 'err) t

  val apply :
    ('a, 'err list) t
    -> (('a -> 'b), 'err list) t
    -> ('b, 'err list) t

  val traverse : ('a -> ('b, 'err) t) -> 'a list -> ('b list, 'err) t
  val sequence : ('ok, 'err) t list -> ('ok list, 'err) t

  module Infix : sig
    val (>>=) : ('a, 'err) t -> ('a -> ('b, 'err) t) -> ('b, 'err) t
    val (>>|) : ('a -> 'b) -> ('a, 'err) t -> ('b, 'err) t
    val (<$>) : ('a -> 'b) -> ('a, 'err) t -> ('b, 'err) t
    val (<*>) :
      ('a, 'err list) t
      -> (('a -> 'b), 'err list) t
      -> ('b, 'err list) t
  end
end = struct
  type ('ok, 'err) t = ('ok, 'err) Result_.result =
    | Ok of 'ok
    | Error of 'err

  let get_ok ?default result =
    match result, default with
    | Ok x, _ -> x
    | Error _, Some x -> x
    | Error _, None  -> failwith "get_ok"

  let get_error = function
    | Ok _ -> failwith "get_error"
    | Error x -> x

  let return x = Ok x
  let ok = return
  let error x = Error x

  let bind result f =
    match result with
    | Ok x -> f x
    | Error x -> Error x

  let map f result = bind result (fun x -> Ok (f x))

  let apply x_result f_result =
    match x_result, f_result with
    | Ok x, Ok f -> Ok (f x)
    | Ok _, Error x -> Error x
    | Error x, Ok _ -> Error x
    | Error x, Error y -> Error (x @ y)

  let traverse f l =
    let rec loop accu l =
      match l with
      | [] -> Ok accu
      | head :: tail ->
        match f head with
        | Ok x -> loop (x :: accu) tail
        | Error x -> Error x
    in
    loop [] l

  let sequence l = traverse (function result -> result) l

  module Infix = struct
    let (>>=) = bind
    let (>>|) = map
    let (<$>) = map
    let (<*>) = apply
  end
end

type ('ok, 'err) t = ('ok, 'err) Result.t =
  | Ok of 'ok
  | Error of 'err

include Result.Infix

include Sexplib.Std

(*let debug x = Printf.fprintf stderr x*)
