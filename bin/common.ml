open! Core
open! Async
open Openapi_spec.Types

let parse_openapi spec =
  return
    (let%map.Or_error parsed = Jsonaf.parse spec in
     Open_api.t_of_jsonaf parsed)
;;

let read_specfile ~file =
  let%bind content = Reader.with_file file ~f:(fun reader -> Reader.contents reader) in
  parse_openapi content
;;
