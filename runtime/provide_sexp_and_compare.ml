open! Core

type nonrec bool = bool [@@deriving sexp, compare]
type nonrec float = float [@@deriving sexp, compare]
type nonrec int64 = int64 [@@deriving sexp, compare]
type nonrec int = int [@@deriving sexp, compare]
type nonrec string = string [@@deriving sexp, compare]
type nonrec 'a list = 'a list [@@deriving sexp, compare]
type nonrec 'a option = 'a option [@@deriving sexp, compare]
