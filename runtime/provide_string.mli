open! Core

(** This module provides string derivations for primitve types. *)

include sig
    type bool [@@deriving string]
    type float [@@deriving string]
    type int64 [@@deriving string]
    type int [@@deriving string]
  end
  with type bool := bool
   and type float := float
   and type int64 := int64
   and type int := int
