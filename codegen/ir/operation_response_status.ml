open! Core
open! Async

module Range = struct
  type t =
    | Informational [@rename "1XX"]
    | Successful [@rename "2XX"]
    | Redirection [@rename "3XX"]
    | Client_error [@rename "4XX"]
    | Server_error [@rename "5XX"]
  [@@deriving compare, string ~case_insensitive]

  include Sexpable.Of_stringable (struct
      type nonrec t = t [@@deriving string]
    end)
end

type t =
  | Default
  | Range of Range.t [@nested ""]
  | Code of
      (Httpaf.Status.t
      [@compare.custom Comparable.lift Int.compare ~f:Httpaf.Status.to_code]) [@fallback]
[@@deriving compare, string ~capitalize:"lowercase"]

let can_be_success_response = function
  | Default | Range Successful -> true
  | Range (Informational | Redirection | Client_error | Server_error) -> false
  | Code code -> Httpaf.Status.is_successful code
;;

let pick_primary_success_response ts =
  let open struct
    type success_comparison =
      | Success_code of int
      | Success_range
      | Default
      | Not_successful
    [@@deriving compare]
  end in
  let best_option =
    List.min_elt
      ts
      ~compare:
        (Comparable.lift [%compare: success_comparison] ~f:(function
          | Range Successful -> Success_range
          | Code code ->
            if Httpaf.Status.is_successful code
            then Success_code (Httpaf.Status.to_code code)
            else Not_successful
          | Default -> Default
          | Range (Informational | Redirection | Client_error | Server_error) ->
            Not_successful))
  in
  let%bind.Option best_option in
  Option.some_if (can_be_success_response best_option) best_option
;;

include Sexpable.Of_stringable (struct
    type nonrec t = t [@@deriving string]
  end)

include Comparable.Make_plain (struct
    type nonrec t = t [@@deriving sexp_of, compare]
  end)
