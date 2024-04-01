open! Core
open! Async
open Common
open Deferred.Or_error.Let_syntax
open Openapi_spec.Types

module Color_print = struct
  let reset = "\027[0m"
  let color c = [%string "\027[%{c#Int}m"]
  let green s = color 32 ^ s ^ reset
  let blue s = color 34 ^ s ^ reset
  let red s = color 31 ^ s ^ reset
  let yellow s = color 33 ^ s ^ reset
  let bold s = "\027[1m" ^ s ^ reset
end

let color_method = function
  | "GET" -> Color_print.green "GET"
  | "POST" -> Color_print.blue "POST"
  | "PUT" -> Color_print.yellow "PUT"
  | "DELETE" -> Color_print.red "DELETE"
  | s -> s
;;

let paths_command =
  Command.async_or_error
    ~summary:"summarize API endpoints"
    [%map_open.Command
      let file =
        flag "spec" (required Filename_unix.arg_type) ~doc:"FILE a v3.0.3 specfile"
      in
      fun () ->
        let%map spec = read_specfile ~file in
        let paths = Open_api.paths spec in
        Map.iteri paths ~f:(fun ~key:endpoint ~data:path_item ->
          print_endline (Color_print.bold endpoint);
          let ops = Path_item.all_operations path_item in
          let () =
            List.iter ops ~f:(fun (http_method, op) ->
              let summary =
                Operation.summary op |> Option.value ~default:"<no summary>"
              in
              let description =
                Operation.description op
                |> Option.map ~f:(fun description -> "\n\t" ^ description)
                |> Option.value ~default:""
              in
              print_endline
                (color_method (Httpaf.Method.to_string http_method)
                 ^ ": "
                 ^ summary
                 ^ description))
          in
          print_newline ())]
;;

let types_command =
  Command.async_or_error
    ~summary:"summarize types"
    [%map_open.Command
      let file =
        flag "spec" (required Filename_unix.arg_type) ~doc:"FILE a v3.0.3 specfile"
      in
      fun () ->
        let%map spec = read_specfile ~file in
        let paths = Open_api.paths spec in
        let components = Open_api.components spec in
        let _operation_list, type_space =
          Openapi_codegen.Generator.make_operation_method_list
            ~type_space:Openapi_codegen_ir.Typify.Type_space.empty
            ~components
            ~paths
        in
        let typespace = Openapi_codegen_ir.Typify.Type_space.to_map type_space in
        Map.filter typespace ~f:(fun v ->
          match Openapi_codegen_ir.Typify.Type.structure v with
          | Existing_type | List _ | Set _ -> false
          | _ -> true)
        |> Map.iter ~f:(fun v -> print_s [%sexp (v : Openapi_codegen_ir.Typify.Type.t)])]
;;

let command : Command.t =
  Command.group
    ~summary:"parse and display summarized information about a spec"
    [ "paths", paths_command; "types", types_command ]
;;
