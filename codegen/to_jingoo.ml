open! Core

type 'a t = 'a -> Jingoo.Jg_types.tvalue

module Export = struct
  open! Jingoo.Jg_types

  let to_jingoo_int int = Tint int
  let to_jingoo_string string = Tstr string
  let to_jingoo_bool bool = Tbool bool
  let to_jingoo_list to_jingoo_a list = Tlist (List.map ~f:to_jingoo_a list)

  let to_jingoo_option to_jingoo_a option =
    match option with
    | None -> Tnull
    | Some value -> to_jingoo_a value
  ;;
end

module Derive = struct
  open Ppx_derive_at_runtime_lib

  (* We must define the derived value type and attribute types we are using. *)
  module Value = struct
    type nonrec 'a t = 'a t
    type _ attribute = Nothing.t
    type (_, _) label_attribute = [ `Key of string ]
    type (_, _) row_attribute = Nothing.t
    type (_, _) constructor_attribute = Nothing.t
    type 'a override = 'a t
  end

  module Types = Types (Value)
  open Types

  module Record_fold = struct
    type ('whole, _) t =
      'whole
      -> (string * Jingoo.Jg_types.tvalue) list
      -> (string * Jingoo.Jg_types.tvalue) list
  end

  module Fold_record = Record.Fold (Record_fold)

  let record (Record.T root) =
    let parts =
      Fold_record.fold
        ~leaf:
          { on_leaf =
              (fun { name; value; attribute; access } x fields ->
                let key =
                  match attribute with
                  | None -> name
                  | Some (`Key key) -> key
                in
                (key, value (access x)) :: fields)
          }
        ~node:{ on_node = (fun left right x fields -> left x (right x fields)) }
        root.tree
    in
    fun x -> Jingoo.Jg_types.Tobj (parts x [])
  ;;

  module Tuple_fold = struct
    type ('whole, _) t =
      'whole -> Jingoo.Jg_types.tvalue list -> Jingoo.Jg_types.tvalue list
  end

  module Fold_tuple = Tuple.Fold (Tuple_fold)

  let tuple (Tuple.T root) =
    let parts =
      Fold_tuple.fold
        ~leaf:
          { on_leaf =
              (fun { index = _; value; access } x members -> value (access x) :: members)
          }
        ~node:{ on_node = (fun left right x members -> left x (right x members)) }
        root.tree
    in
    (* Array is better for nth access *)
    fun x -> Jingoo.Jg_types.Tarray (Array.of_list (parts x []))
  ;;

  let empty make_nothing empty = make_nothing empty |> Nothing.unreachable_code

  (* not really needed, but if we really want to implement variants, we can do something
     like an object with jingoo_variant_kind, all possibilities are variants, empty
     variants are bool, and everything else is nullable. *)
  let variant _ = failwith "Not implemented"
  let poly_variant _ = failwith "Not implemented"
  let with_attribute _ attribute = Nothing.unreachable_code attribute
  let recursive _name t = force t
  let override t = t
end
