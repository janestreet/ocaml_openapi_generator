(** Parsing of OpenAPI path templates such as ["/v1/delete/{id}"]. This library has no
    dependencies (and uses no ppx) so that [ppx_openapi] can use it without linking the
    full [Openapi_runtime]. [Openapi_runtime.Path_template] re-exports these types with
    [sexp] and [equal] derived. *)

module Path_component : sig
  type t =
    | Constant of string
    | Parameter of string
end

type t = Path_component.t list

val of_template_string : string -> t option
