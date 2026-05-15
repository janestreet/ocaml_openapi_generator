open! Core

type t = (string * string list) list

let concat_to_list = List.concat
let singleton f ~key ~value = [ key, [ f value ] ]
let make_nullable f value = Option.value_map ~f value ~default:"null"

let optional f ~key ~value =
  match value with
  | None -> []
  | Some value -> f ~key ~value
;;

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

let map
  ?(explode = true)
  ?(style = Openapi_spec.Types.Parameter.Query_style.Form)
  f
  ~key
  ~value
  =
  match explode, style with
  | true, Form | (true | false), (Space_delimited | Pipe_delimited) ->
    List.map value ~f:(fun (key, value) -> key, [ f value ])
  | false, Form ->
    [ key, List.concat_map value ~f:(fun (key, value) -> [ key; f value ]) ]
  | (true | false), Deep_object ->
    List.map value ~f:(fun (inner_key, value) ->
      [%string {|%{key}[%{inner_key}]|}], [ f value ])
;;

let object_ ?explode ?style () ~key ~value =
  map ?explode ?style Jsonaf.to_string ~key ~value
;;
