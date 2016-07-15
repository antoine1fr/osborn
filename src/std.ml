module Result_ = Result

module Result = struct
  include Result_

  type ('ok, 'err) t = ('ok, 'err) result

  let get_ok ?default result =
    match result, default with
    | Ok x, _ -> x
    | Error _, Some x -> x
    | Error _, None  -> failwith "get_ok"
end

include Sexplib.Std
