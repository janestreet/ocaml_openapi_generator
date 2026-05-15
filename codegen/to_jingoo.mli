open! Core

include
  Ppx_derive_at_runtime_lib.S
  with type 'a t = 'a -> Jingoo.Jg_types.tvalue
   and type (_, _) Derive.Value.label_attribute = [ `Key of string ]

module Export : sig
  val to_jingoo_int : int -> Jingoo.Jg_types.tvalue
  val to_jingoo_string : string -> Jingoo.Jg_types.tvalue
  val to_jingoo_bool : bool -> Jingoo.Jg_types.tvalue
  val to_jingoo_list : ('a -> Jingoo.Jg_types.tvalue) -> 'a list -> Jingoo.Jg_types.tvalue

  val to_jingoo_option
    :  ('a -> Jingoo.Jg_types.tvalue)
    -> 'a option
    -> Jingoo.Jg_types.tvalue
end
