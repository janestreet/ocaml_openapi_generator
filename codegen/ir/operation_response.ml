open! Core
open! Async
open Typify

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
  ; success_response_for_operation : bool
  }
[@@deriving fields ~getters ~setters ~iterators:create, sexp]

let create = Fields.create
let get_success_response_for_operation = List.find ~f:success_response_for_operation

let%expect_test "of_string basic test" =
  List.iter [ "2xx"; "default"; "204"; "200" ] ~f:(fun status ->
    let parsed_status = Operation_response_status.of_string status in
    print_s [%message status (parsed_status : Operation_response_status.t)]);
  return
    [%expect
      {|
      (2xx (parsed_status 2XX))
      (default (parsed_status default))
      (204 (parsed_status 204))
      (200 (parsed_status 200))
      |}]
;;
