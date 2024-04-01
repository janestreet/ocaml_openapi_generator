open! Core
open! Async
open Typify

module Operation_response_status = struct
  type t =
    | Code of Httpaf_sexpable.Status.t
    | Range of int
    | Default
  [@@deriving sexp]

  let of_string = function
    | "default" -> Default
    | "1XX" | "1xx" -> Range 1
    | "2XX" | "2xx" -> Range 2
    | "3XX" | "3xx" -> Range 3
    | "4XX" | "4xx" -> Range 4
    | "5XX" | "5xx" -> Range 5
    | other -> Code (Httpaf_sexpable.Status.of_string other)
  ;;
end

module Operation_response_type = struct
  type t =
    | Resolved of Type_id.t
    | None
    | Raw
    | Upgrade
  [@@deriving sexp]
end

type t =
  { status_code : Operation_response_status.t
  ; type_id : Operation_response_type.t
  ; description : string option
  }
[@@deriving fields ~getters ~setters ~iterators:create, sexp]

let create = Fields.create

let is_default t =
  match t.status_code with
  | Default -> true
  | Code c -> Httpaf_sexpable.Status.is_successful c
  | _ -> false
;;

let get_default = List.find ~f:is_default

let%expect_test "of_string basic test" =
  List.iter [ "2xx"; "default"; "204"; "200" ] ~f:(fun status ->
    let parsed_status = Operation_response_status.of_string status in
    print_s [%message status (parsed_status : Operation_response_status.t)]);
  return
    [%expect
      {|
      (2xx (parsed_status (Range 2)))
      (default (parsed_status Default))
      (204 (parsed_status (Code 204)))
      (200 (parsed_status (Code 200)))
      |}]
;;
