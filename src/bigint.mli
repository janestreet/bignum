open! Core_kernel

(* Arbitrary-precision integers based on Zarith 1.4.
   This implementation should be significantly faster and use less memory than [Big_int].
   See benchmarks labeled "vs. Big_int" in the implementation. *)
type t

(** [gen] produces integers representable within [Quickcheck.size] bytes, with a random
    sign. *)
include Int_intf.S_unbounded with type t := t

val to_int64_exn : t -> Int64.t

val to_int       : t -> int       option
val to_int32     : t -> Int32.t   option
val to_int64     : t -> Int64.t   option
val to_nativeint : t -> nativeint option

val of_int       : int       -> t
val of_int32     : Int32.t   -> t
val of_int64     : Int64.t   -> t
val of_nativeint : nativeint -> t

val to_zarith_bigint : t -> Zarith_1_4.Z.t
val of_zarith_bigint : Zarith_1_4.Z.t -> t

(** [random t] produces a value uniformly distributed between [zero] (inclusive) and
    [t] (exclusive), or raises if [t <= zero]. *)
val random : ?state:Random.State.t -> t -> t

(* [gen_positive] and [gen_negative] produce integers with a maximum number of digits one
   greater than the size value passed in by [Quickcheck.test], with the appropriate sign.
   Neither generator produces zero. *)
val gen_positive : t Quickcheck.Generator.t
val gen_negative : t Quickcheck.Generator.t

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving sexp, bin_io, compare]
  end
end
