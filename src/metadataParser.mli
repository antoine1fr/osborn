module Map = Map.Make (String)

type metadata = string Map

type metadata = string Map.t
val parse : string -> (metadata * string)
