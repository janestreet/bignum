open Core_kernel.Std

(* Arbitrary-precision integers based on Zarith 1.4.
   This implementation should be significantly faster and use less memory than [Big_int].
   See benchmarks labeled "vs. Big_int" in the implementation. *)
type t

include Int_intf.S with type t := t

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

val num_bits            : [`Bigint_is_unbounded]
val min_value           : [`Bigint_is_unbounded]
val max_value           : [`Bigint_is_unbounded]
val shift_right_logical : [`Bigint_is_unbounded]

(** [random t] produces a value uniformly distributed between [zero] (inclusive) and
    [t] (exclusive), or raises if [t <= zero]. *)
val random : ?state:Random.State.t -> t -> t

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving sexp, bin_io, compare]
  end
end
