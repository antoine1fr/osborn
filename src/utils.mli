val ezjsonm_value_of_yojson : Yojson.Safe.json -> Mustache.Json.value
val ezjsonm_of_yojson : Yojson.Safe.json -> Mustache.Json.t
val read_file : string -> (string, string) Std.Result.t
val write_file : string -> string -> (unit, string) Std.Result.t
val get_folder_content : string -> (string list, string) Std.Result.t
val file_extension : string -> string
