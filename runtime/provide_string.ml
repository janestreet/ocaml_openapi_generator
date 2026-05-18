open! Core

type nonrec bool = bool [@@deriving string]
type nonrec float = float [@@deriving string]
type nonrec int64 = Int64.t [@@deriving string]
type nonrec int = int [@@deriving string]
type nonrec string = string [@@deriving string]
