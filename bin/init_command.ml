open! Core
open! Async

let make_relative ~reference path =
  let reference = Filename_unix.realpath reference in
  let path = Filename_unix.realpath path in
  let reference = File_path.Absolute.of_string reference |> File_path.Absolute.to_parts in
  let path = File_path.Absolute.of_string path |> File_path.Absolute.to_parts in
  let rec drop_common_prefix reference path =
    match reference, path with
    | a :: reference, b :: path when File_path.Part.equal a b ->
      drop_common_prefix reference path
    | _ -> reference, path
  in
  let reference_suffix, path_suffix = drop_common_prefix reference path in
  let path_to_shared_prefix_from_reference =
    List.map reference_suffix ~f:(const File_path.Part.dot_dot)
  in
  File_path.Relative.of_parts_exn (path_to_shared_prefix_from_reference @ path_suffix)
  |> File_path.Relative.to_string
;;

module Source = struct
  type t = { spec_file : string }

  let param =
    [%map_open.Command
      let spec_file =
        flag "-spec" (required Filename_unix.arg_type) ~doc:"FILE path to spec file"
      in
      fun ~destination -> { spec_file = make_relative ~reference:destination spec_file }]
  ;;

  let setup ~source:{ spec_file } = return spec_file
  let description { spec_file } = [%string "local file `%{spec_file}`"]

  let upgrade_instructions { spec_file } =
    [%string
      {|To upgrade to a new schema:

1. Replace the schema file at `%{spec_file}`

2. Rebuild to regenerate the library. Make sure you accept any dune file correction first.|}]
  ;;

  let next_steps_extra (_ : t) = ()
end

let validate_library_name name =
  if String.is_empty name
  then Or_error.error_string "Library name cannot be empty"
  else if not (Char.is_alpha (String.get name 0))
  then Or_error.error_string "Library name must start with a letter"
  else if not (String.for_all name ~f:(fun c -> Char.is_alphanum c || Char.( = ) c '_'))
  then
    Or_error.error_string
      "Library name must contain only alphanumeric characters and underscores"
  else Ok ()
;;

let create_readme ~destination ~name ~source =
  let source_info = Source.description source in
  let upgrade_instructions = Source.upgrade_instructions source in
  let content =
    [%string
      {|---
title: %{name}
uuid: %{Uuid.create_random Random.State.default#Uuid}
---

# %{name}

This is an auto-generated OCaml library from an OpenAPI specification.

## Source

This library was generated from %{source_info}.

## Contents

- Generated OCaml types for all schemas in the OpenAPI spec
- Generated OCaml client functions for all API endpoints
- Type-safe request/response handling

## Upgrading the Schema

%{upgrade_instructions}
|}]
  in
  Writer.save (destination ^/ "README.md") ~contents:content
;;

let confirm_code_gen_flags ~(code_gen : Openapi_codegen.Config.Code_gen.t) =
  Core.print_endline "Code generation flags:";
  Core.print_s [%sexp (code_gen : Openapi_codegen.Config.Code_gen.t)];
  Core.print_endline "";
  Core.print_endline "Run with -help to see all available flags.";
  Async_interactive.ask_yn "Proceed with these flags?"
;;

let command =
  Command.async_or_error
    ~summary:"initialize a new OpenAPI-generated library"
    (let%map_open.Command name = flag "-name" (required string) ~doc:"STRING library name"
     and destination =
       flag "-out" (required Filename_unix.arg_type) ~doc:"DIR output directory"
     and code_gen = Openapi_codegen.Config.Code_gen.param
     and source = Source.param in
     fun () ->
       let%bind () = Unix.mkdir ~p:() destination in
       let source = source ~destination in
       let open Deferred.Or_error.Let_syntax in
       (* Validate library name *)
       let%bind () = Deferred.return (validate_library_name name) in
       (* Confirm code gen flags *)
       let%bind confirmed = confirm_code_gen_flags ~code_gen |> Deferred.ok in
       if not confirmed
       then Deferred.Or_error.error_string "Aborted by user"
       else (
         (* Create destination directory *)
         let%bind () = Unix.mkdir ~p:() destination |> Deferred.ok in
         (* Set up schema source and get spec path *)
         let%bind spec_file = Source.setup source |> Deferred.ok in
         (* Create README *)
         let%bind () = create_readme ~destination ~name ~source |> Deferred.ok in
         (* Create initial jbuild using make_jbuild with empty paths *)
         let config =
           Openapi_codegen.Config.create
             ~name
             ~destination:"./"
               (* The destination is the current directory from the library's perspective. *)
             ~spec_file
             ~generated_files_archive:(Some "files.tar.tmp")
             ~code_gen
         in
         let jbuild_content =
           Openapi_codegen.Generator.make_jbuild ~config ~paths:String.Set.empty
         in
         let%bind () =
           Writer.save (destination ^/ "dune") ~contents:jbuild_content |> Deferred.ok
         in
         Core.print_endline "";
         Core.print_endline "==========================================";
         Core.print_endline "   Next Steps";
         Core.print_endline "==========================================";
         Core.print_endline "";
         Core.print_endline [%string "Generated library scaffolding at: %{destination}"];
         Core.print_endline "";
         Core.print_endline "To complete setup:";
         Core.print_endline "";
         Core.print_endline "  - Build to generate the library files.";
         ();
         Deferred.Or_error.return ()))
;;
