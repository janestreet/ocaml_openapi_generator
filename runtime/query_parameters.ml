open! Core

type t = (string * string list) list

let to_list t = t
let concat = List.concat
let singleton f ~key ~value = [ key, [ f value ] ]

let array
  ?(explode = true)
  ?(style = Openapi_spec.Types.Parameter.Query_style.Form)
  f
  ~key
  ~value
  =
  match explode, style with
  | true, (Form | Space_delimited | Pipe_delimited) | (true | false), Deep_object ->
    List.map value ~f:(fun inner -> key, [ f inner ])
  | false, Form -> [ key, List.map value ~f ]
  | false, Space_delimited -> [ key, [ String.concat ~sep:" " (List.map value ~f) ] ]
  | false, Pipe_delimited -> [ key, [ String.concat ~sep:"|" (List.map value ~f) ] ]
;;

let object_
  ?(explode = true)
  ?(style = Openapi_spec.Types.Parameter.Query_style.Form)
  ()
  ~key
  ~value
  =
  match explode, style with
  | true, Form | (true | false), (Space_delimited | Pipe_delimited) ->
    List.map value ~f:(fun (key, json) -> key, [ Jsonaf.to_string json ])
  | false, Form ->
    [ key, List.concat_map value ~f:(fun (key, json) -> [ key; Jsonaf.to_string json ]) ]
  | (true | false), Deep_object ->
    List.map value ~f:(fun (inner_key, json) ->
      [%string {|%{key}[%{inner_key}]|}], [ Jsonaf.to_string json ])
;;
