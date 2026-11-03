open! Core
open! Async
open Option.Let_syntax

module Path_component = struct
  type t = Openapi_path_template.Path_component.t =
    | Constant of string
    | Parameter of string
  [@@deriving sexp, equal]
end

type t = Path_component.t list [@@deriving sexp, equal]

let of_template_string = Openapi_path_template.of_template_string

let encode_path path =
  List.map ~f:Uri.pct_encode path
  |> List.map ~f:(fun path -> [%string "/%{path}"])
  |> String.concat
;;

let render_path ?(parameters = String.Map.empty) (t : t) =
  let%map filled =
    List.map t ~f:(function
      | Constant c -> Some c
      | Parameter p -> Map.find parameters p)
    |> Option.all
  in
  encode_path filled
;;

let%expect_test "render_path_exn basic test" =
  let components =
    [ Path_component.Constant "v1"
    ; Path_component.Constant "delete"
    ; Path_component.Parameter "id"
    ]
  in
  let parameters = String.Map.of_alist_exn [ "id", "example" ] in
  print_endline (render_path ~parameters components |> Option.value_exn);
  [%expect {| /v1/delete/example |}];
  Deferred.return ()
;;
