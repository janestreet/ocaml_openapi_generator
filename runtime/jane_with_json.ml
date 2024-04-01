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
