open! Core
open! Async
open Option.Let_syntax

module Path_component = struct
  type t =
    | Constant of string
    | Parameter of string
  [@@deriving sexp, equal]
end

type t = Path_component.t list [@@deriving sexp, equal]

let of_template_string template =
  let open Path_component in
  String.split ~on:'/' template
  |> List.tl
  |> Option.map ~f:(fun lst ->
    List.map lst ~f:(fun str ->
      let maybe_parameter =
        let%bind str = String.chop_prefix str ~prefix:"{" in
        String.chop_suffix str ~suffix:"}"
      in
      Option.value_map maybe_parameter ~default:(Constant str) ~f:(fun parameter ->
        Parameter parameter)))
;;

let render_path ?(parameters = String.Map.empty) (t : t) =
  let%map filled =
    List.map t ~f:(function
      | Constant c -> Some c
      | Parameter p -> Map.find parameters p)
    |> Option.all
  in
  "/" ^ String.concat ~sep:"/" filled
;;

let%expect_test "of_template_string basic test" =
  let expected =
    [ Path_component.Constant "v1"
    ; Path_component.Constant "delete"
    ; Path_component.Parameter "id"
    ]
  in
  let actual = of_template_string "/v1/delete/{id}" |> Option.value_exn in
  let is_equal = equal expected actual in
  print_s [%sexp (is_equal : bool)];
  [%expect {| true |}];
  Deferred.return ()
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
