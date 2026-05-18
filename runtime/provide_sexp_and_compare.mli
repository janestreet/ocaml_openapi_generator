open! Core

(** This module provides sexp, compare derivations for primitve types. *)

include sig
    type bool [@@deriving sexp, compare]
    type float [@@deriving sexp, compare]
    type int64 [@@deriving sexp, compare]
    type int [@@deriving sexp, compare]
    type string [@@deriving sexp, compare]
    type 'a list [@@deriving sexp, compare]
    type 'a option [@@deriving sexp, compare]
  end
  with type bool := bool
   and type float := float
   and type int64 := int64
   and type int := int
   and type string := string
   and type 'a list := 'a list
   and type 'a option := 'a option
