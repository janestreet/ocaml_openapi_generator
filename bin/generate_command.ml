open! Core
open! Async
open Common
open Deferred.Or_error.Let_syntax

let command =
  Command.async_or_error
    ~summary:"generate an OCaml library into a directory"
    [%map_open.Command
      let file =
        flag "spec" (required Filename_unix.arg_type) ~doc:"FILE a v3.0.3 specfile"
      and destination =
        flag "out" (required Filename_unix.arg_type) ~doc:"DIR the output directory"
      and name = flag "name" (required string) ~doc:"STRING project name" in
      let config = Openapi_codegen.Config.create ~name ~destination in
      fun () ->
        let%bind api = read_specfile ~file in
        Openapi_codegen.Generator.make_files ~config ~api]
;;
