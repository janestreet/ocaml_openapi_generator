open! Core
open! Async
open Jsonaf.Export

module Jsonaf_int64 = struct
  include Int64

  let jsonaf_of_t = jsonaf_of_int64
  let t_of_jsonaf = int64_of_jsonaf
end

module Jsonaf_int = struct
  include Int

  let jsonaf_of_t = jsonaf_of_int
  let t_of_jsonaf = int_of_jsonaf
end

module Jsonaf_float = struct
  include Float.Stable.V1

  let to_string = Float.to_string
  let jsonaf_of_t = jsonaf_of_float
  let t_of_jsonaf = float_of_jsonaf
end

module Jsonaf_bool = struct
  include Bool.Stable.V1

  let to_string = Bool.to_string
  let jsonaf_of_t = jsonaf_of_bool
  let t_of_jsonaf = bool_of_jsonaf
end

module Jsonaf_ip = struct
  include Unix.Inet_addr.Stable.V1
  include (Unix.Inet_addr : Stringable.S with type t := t)

  let jsonaf_of_t t = `String (Unix.Inet_addr.to_string t)

  let t_of_jsonaf = function
    | `String str -> Unix.Inet_addr.of_string str
    | _ -> failwith "IP Address must be JSON string"
  ;;
end

let classify_inet_addr inet_addr =
  match Unix.Inet_addr.to_string inet_addr with
  | ip when String.contains ip ':' -> `Ipv6
  | _ -> `Ipv4
;;

module Jsonaf_ipv4 = struct
  include Unix.Inet_addr.Stable.V1
  include (Unix.Inet_addr : Stringable.S with type t := t)

  let jsonaf_of_t t = `String (Unix.Inet_addr.to_string t)

  let t_of_jsonaf json =
    match json with
    | `String ip ->
      let addr = Unix.Inet_addr.of_string ip in
      (match classify_inet_addr addr with
       | `Ipv4 -> addr
       | `Ipv6 -> raise_s [%message "Invalid IPv4 address"])
    | _ -> failwith "IP Address must be JSON string"
  ;;
end

module Jsonaf_ipv6 = struct
  include Unix.Inet_addr.Stable.V1
  include (Unix.Inet_addr : Stringable.S with type t := t)

  let jsonaf_of_t t = `String (Unix.Inet_addr.to_string t)

  let t_of_jsonaf json =
    match json with
    | `String ip ->
      let addr = Unix.Inet_addr.of_string ip in
      (match classify_inet_addr addr with
       | `Ipv6 -> addr
       | `Ipv4 -> raise_s [%message "Invalid IPv6 address"])
    | _ -> raise_s [%message "Expected a string"]
  ;;
end

module Jsonaf_time = struct
  include Time_ns_unix.Stable.V1

  let to_string = Time_ns_unix.to_string_iso8601_basic ~zone:Time_ns_unix.Zone.utc
  let of_string = Time_ns.of_string
  let jsonaf_of_t t = `String (to_string t)

  let t_of_jsonaf = function
    | `String time -> Time_ns_unix.of_string time
    | _ -> failwith "Time must be string in JSON"
  ;;
end

module Jsonaf_uuid = struct
  include Uuid.Stable.V1

  let t_of_jsonaf = function
    | `String s -> Uuid.of_string s
    | _ -> failwith "Uuids must be JSON strings"
  ;;

  let jsonaf_of_t t = `String (Uuid.to_string t)
end

module Jsonaf_string = struct
  include String.Stable.V1

  let t_of_jsonaf = string_of_jsonaf
  let jsonaf_of_t = jsonaf_of_string
end

module Assoc = struct
  type ('key, 'value) t = ('key * 'value) list [@@deriving sexp, compare]

  module M (Key : sig
      type t
    end) =
  struct
    type nonrec 'value t = (Key.t, 'value) t
  end

  module type Jsonaf_of_m = sig
    type t [@@deriving to_string]
  end

  let jsonaf_of_m__t
    (type key)
    (module M : Jsonaf_of_m with type t = key)
    jsonaf_of_value
    values
    : Jsonaf.t
    =
    `Object (List.map values ~f:(fun (k, v) -> M.to_string k, jsonaf_of_value v))
  ;;

  module type M_of_jsonaf = sig
    type t [@@deriving of_string]
  end

  let m__t_of_jsonaf
    (type key)
    (module M : M_of_jsonaf with type t = key)
    value_of_jsonaf
    (json : Jsonaf.t)
    =
    match json with
    | `Object lst -> List.map lst ~f:(fun (k, v) -> M.of_string k, value_of_jsonaf v)
    | _ ->
      Jsonaf.Conv.of_jsonaf_error
        "Openapi_runtime.Jane_with_json.Assoc.m__t_of_jsonaf: expected a JSON object"
        json
  ;;

  module type Sexp_of_m = sig
    type t [@@deriving sexp_of]
  end

  let sexp_of_m__t (type key) (module M : Sexp_of_m with type t = key) sexp_of_value value
    =
    sexp_of_t M.sexp_of_t sexp_of_value value
  ;;

  module type M_of_sexp = sig
    type t [@@deriving of_sexp]
  end

  let m__t_of_sexp (type key) (module M : M_of_sexp with type t = key) value_of_sexp sexp =
    t_of_sexp M.t_of_sexp value_of_sexp sexp
  ;;

  module type Compare_m = sig
    type t [@@deriving compare]
  end

  let compare_m__t (type key) (module M : Compare_m with type t = key) compare_value a b =
    compare M.compare compare_value a b
  ;;
end

module With_additional_properties (Serializable : sig
    type result
    type t [@@deriving jsonaf]

    val jsonaf_fields_of_t : string list

    module Additional_properties_content : sig
      type t [@@deriving jsonaf]
    end

    val of_serializable : t -> (string, Additional_properties_content.t) Assoc.t -> result
    val to_serializable : result -> t * (string, Additional_properties_content.t) Assoc.t
  end) =
struct
  let predefined_properties = String.Set.of_list Serializable.jsonaf_fields_of_t

  let jsonaf_of_t result =
    let serializable, additional_properties = Serializable.to_serializable result in
    let additional_properties =
      List.Assoc.map
        additional_properties
        ~f:Serializable.Additional_properties_content.jsonaf_of_t
    in
    match Serializable.jsonaf_of_t serializable with
    | `Object value -> `Object (value @ additional_properties)
    | json ->
      raise_s
        [%message
          "With_additional_properties: Serializable.jsonaf_of_t returned an unexpected \
           json, excepted an object"
            (json : Jsonaf.t)]
  ;;

  let t_of_jsonaf json =
    match json with
    | `Object json ->
      let properties, additional_properties =
        List.partition_tf json ~f:(fun (key, _) -> Set.mem predefined_properties key)
      in
      let serializable = Serializable.t_of_jsonaf (`Object properties) in
      let additional_properties =
        List.Assoc.map
          additional_properties
          ~f:Serializable.Additional_properties_content.t_of_jsonaf
      in
      Serializable.of_serializable serializable additional_properties
    | json ->
      Jsonaf.Conv.of_jsonaf_error
        "With_additional_properties.t_of_jsonaf received an unexpected json, expected an \
         object"
        json
  ;;
end
