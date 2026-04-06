open! Core
open! Async
open Common
open Deferred.Or_error.Let_syntax

let command =
  Command.async_or_error
    ~summary:"generate an OCaml library into a directory"
    (let%map_open.Command config = Openapi_codegen.Config.param in
     fun () ->
       let%bind api = read_specfile ~file:config.spec_file in
       Openapi_codegen.Generator.make_files ~config ~api)
;;
