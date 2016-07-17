module Map = Map.Make (String)

type metadata = string Map.t

let parse str =
