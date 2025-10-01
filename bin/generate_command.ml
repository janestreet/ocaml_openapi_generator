open! Core
open! Async
open Common
open Deferred.Or_error.Let_syntax

let command =
  Command.async_or_error
    ~summary:"generate an OCaml library into a directory"
    [%map_open.Command
      let spec_file =
        flag "spec" (required Filename_unix.arg_type) ~doc:"FILE a v3.0.3 specfile"
      and destination =
        flag "out" (required Filename_unix.arg_type) ~doc:"DIR the output directory"
      and name = flag "name" (required string) ~doc:"STRING project name"
      and raise_on_optional_null =
        flag
          "raise-on-optional-null"
          no_arg
          ~doc:"Use the legacy behavior of raising if an optional field contains null"
      and include_unknown_fallback_for_enums =
        flag
          "include-unknown-fallback-for-enums"
          no_arg
          ~doc:
            "Include an unknown fallback for enums instead of raising if an unexpected \
             value is found"
      in
      let config = Openapi_codegen.Config.create ~name ~destination in
      fun () ->
        let%bind api = read_specfile ~file:spec_file in
        Openapi_codegen.Generator.make_files
          ~config
          ~api
          ~spec_file
          ~raise_on_optional_null
          ~include_unknown_fallback_for_enums]
;;
