open! Core
open! Async
open! Openapi_runtime.Jane_with_json
open! Jsonaf.Export

type t = unit [@@deriving sexp]

let t_of_jsonaf _ = ()
let jsonaf_of_t () = `Null
