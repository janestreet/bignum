open Core
module Z = Zarith.Z

(* Define [type t] via include to avoid error about type names being unique. *)
module T = struct
  type t : value mod contended global portable = { global_ global : Z.t }
  [@@unboxed] [@@unsafe_allow_any_mode_crossing] [@@deriving typerep ~abstract]

  [%%template
  [@@@mode.default m = (local, global)]

  let[@inline] compare x y = Z.compare x.global y.global
  let[@inline] equal x y = Z.equal x.global y.global]

  let hash_fold_t state t = hash_fold_int state (Z.hash t.global)
  let[@inline] hash t = Z.hash t.global
end

include T

let module_name = "Bigint"
let invariant (_ : t) = ()

module Stringable_t = struct
  type nonrec t = t

  let%template[@alloc __ = (heap, stack)] [@inline] to_string { global = t } =
    Z.to_string t
  ;;

  let of_string_base str ~name ~of_string =
    try { global = of_string str } with
    | _ ->
      failwithf "%s.%s: invalid argument %S" name module_name (globalize_string str) ()
  ;;

  let of_string str = of_string_base str ~name:"of_string" ~of_string:Z.of_string
end

module Stable = struct
  module V1 = struct
    module Bin_rep = struct
      open Stable_witness.Export

      type t =
        | Zero
        | Pos of string
        | Neg of string
      [@@deriving bin_io ~localize, stable_witness]
    end

    module Bin_rep_conversion = struct
      type nonrec t = t

      let%template[@mode m = (global, local)] to_binable { global = t } =
        let s = Z.sign t in
        if s > 0
        then (
          let bits = Z.to_bits t in
          Bin_rep.Pos bits [@exclave_if_local m])
        else if s < 0
        then (
          let bits = Z.to_bits t in
          Bin_rep.Neg bits [@exclave_if_local m])
        else Bin_rep.Zero
      ;;

      let of_binable = function
        | Bin_rep.Zero -> { global = Z.zero }
        | Bin_rep.Pos bits -> { global = Z.of_bits bits }
        | Bin_rep.Neg bits -> { global = Z.of_bits bits |> Z.neg }
      ;;
    end

    type nonrec t = t [@@deriving compare ~localize, equal ~localize, hash]

    include%template
      Sexpable.Stable.Of_stringable.V1 [@alloc stack] [@modality portable] (Stringable_t)

    include%template
      Binable.Stable.Of_binable.V1 [@mode local] [@modality portable] [@alert "-legacy"]
        (Bin_rep)
        (Bin_rep_conversion)

    let%expect_test "stable" =
      print_endline [%bin_digest: t];
      [%expect {| 2872d29354b873e996296d440fad2e52 |}]
    ;;

    let stable_witness : t Stable_witness.t =
      let (_bin_io : t Stable_witness.t) =
        (* Binable.Stable.of_binable.V1 *)
        Stable_witness.of_serializable
          Bin_rep.stable_witness
          Bin_rep_conversion.of_binable
          Bin_rep_conversion.to_binable
      in
      let (_sexp : t Stable_witness.t) = Stable_witness.assert_stable in
      Stable_witness.assert_stable
    ;;
  end

  module V2 = struct
    type nonrec t = t [@@deriving compare ~localize, equal ~localize]

    include%template Sexpable.Stable.Of_stringable.V1 [@modality portable] (Stringable_t)

    let compute_size_in_bytes x =
      let numbits = Z.numbits x in
      Int.round_up ~to_multiple_of:8 numbits / 8
    ;;

    let compute_tag ~size_in_bytes ~negative =
      let open Int63 in
      let sign_bit = if negative then one else zero in
      (* Can't overflow: size <= String.length bits < 2 * max_string_length < max_int63
      *)
      shift_left (of_int size_in_bytes) 1 + sign_bit
    ;;

    [%%template
    [@@@mode.default m = (global, local)]

    let bin_size_t : (t Bin_prot.Size.sizer[@mode local]) =
      fun { global = x } ->
      let size_in_bytes = compute_size_in_bytes x in
      if size_in_bytes = 0
      then Int63.bin_size_t Int63.zero
      else (
        let negative = Z.sign x = -1 in
        let tag = compute_tag ~size_in_bytes ~negative in
        Int63.bin_size_t tag + size_in_bytes)
    ;;

    let bin_write_t : (t Bin_prot.Write.writer[@mode local]) =
      fun buf ~pos { global = x } ->
      let size_in_bytes = compute_size_in_bytes x in
      if size_in_bytes = 0
      then Int63.bin_write_t buf ~pos Int63.zero
      else (
        let bits = Z.to_bits x in
        let negative = Z.sign x = -1 in
        let tag = compute_tag ~size_in_bytes ~negative in
        let pos = Int63.bin_write_t buf ~pos tag in
        Bin_prot.Common.blit_string_buf bits ~dst_pos:pos buf ~len:size_in_bytes;
        pos + size_in_bytes)
    ;;]

    let bin_read_t : t Bin_prot.Read.reader =
      fun buf ~pos_ref ->
      let tag = Core.Int63.bin_read_t buf ~pos_ref in
      if Int63.equal tag Int63.zero
      then { global = Z.zero }
      else (
        let negative = Int63.(tag land one = one) in
        let size_in_bytes = Int63.(to_int_exn (shift_right tag 1)) in
        (* Even though we could cache a buffer for small sizes, the extra logic leads to a
           decrease in performance *)
        let bytes = Bytes.create size_in_bytes in
        Bin_prot.Common.blit_buf_bytes ~src_pos:!pos_ref buf bytes ~len:size_in_bytes;
        let abs =
          Z.of_bits (Bytes.unsafe_to_string ~no_mutation_while_string_reachable:bytes)
        in
        pos_ref := !pos_ref + size_in_bytes;
        { global = (if negative then Z.neg abs else abs) })
    ;;

    let module_name = "Bigint.Stable.V2.t"

    let bin_writer_t : t Bin_prot.Type_class.writer =
      { size = bin_size_t; write = bin_write_t }
    ;;

    let __bin_read_t__ _buf ~pos_ref _vint =
      Bin_prot.Common.raise_variant_wrong_type module_name !pos_ref
    ;;

    let bin_reader_t : t Bin_prot.Type_class.reader =
      { read = bin_read_t; vtag_read = __bin_read_t__ }
    ;;

    let bin_shape_t : Bin_prot.Shape.t =
      Bin_prot.Shape.basetype
        (Bin_prot.Shape.Uuid.of_string "7a8cceb2-f3a2-11e9-b7cb-aae95a547ff6")
        []
    ;;

    let bin_t : t Bin_prot.Type_class.t =
      { shape = bin_shape_t; writer = bin_writer_t; reader = bin_reader_t }
    ;;

    let stable_witness : t Stable_witness.t =
      let (_bin_io : t Stable_witness.t) =
        (* implemented directly above *)
        Stable_witness.assert_stable
      in
      let (_sexp : t Stable_witness.t) = Stable_witness.assert_stable in
      Stable_witness.assert_stable
    ;;
  end
end

module Unstable = struct
  include Stable.V1
  include Stringable_t

  let globalize { global = t } = { global = t }

  let of_string_opt (local_ t) =
    try Some (of_string t) with
    | _ -> None
  ;;

  let (t_sexp_grammar : t Sexplib.Sexp_grammar.t) = { untyped = Integer }
  let of_zarith_bigint t = { global = t }
  let to_zarith_bigint { global = t } = t

  let ( /% ) (local_ x) (local_ y) =
    if Z.sign y.global >= 0
    then { global = Z.ediv x.global y.global }
    else
      failwithf
        "%s.(%s /%% %s) : divisor must be positive"
        module_name
        (to_string x)
        (to_string y)
        ()
  ;;

  let ( % ) (local_ x) (local_ y) =
    if Z.sign y.global >= 0
    then { global = Z.erem x.global y.global }
    else
      failwithf
        "%s.(%s %% %s) : divisor must be positive"
        module_name
        (to_string x)
        (to_string y)
        ()
  ;;

  let[@inline] ( - ) { global = x } { global = y } = { global = Z.( - ) x y }
  let[@inline] ( + ) { global = x } { global = y } = { global = Z.( + ) x y }
  let[@inline] ( * ) { global = x } { global = y } = { global = Z.( * ) x y }
  let[@inline] ( / ) { global = x } { global = y } = { global = Z.( / ) x y }
  let[@inline] rem { global = x } { global = y } = { global = Z.rem x y }
  let[@inline] ( ~- ) { global = x } = { global = Z.( ~- ) x }
  let[@inline] neg (local_ { global = x }) = { global = Z.neg x }
  let[@inline] abs_local (local_ { global = x }) = { global = Z.abs x }
  let abs = abs_local
  let[@inline] succ (local_ { global = x }) = { global = Z.succ x }
  let[@inline] pred (local_ { global = x }) = { global = Z.pred x }
  let[@inline] equal__local (local_ { global = x }) (local_ { global = y }) = Z.equal x y
  let[@inline] equal x y = equal__local x y
  let ( = ) = equal
  let[@inline] ( < ) { global = x } { global = y } = Z.lt x y
  let[@inline] ( > ) { global = x } { global = y } = Z.gt x y
  let[@inline] ( <= ) { global = x } { global = y } = Z.leq x y
  let[@inline] ( >= ) { global = x } { global = y } = Z.geq x y
  let[@inline] max { global = x } { global = y } = { global = Z.max x y }
  let[@inline] min { global = x } { global = y } = { global = Z.min x y }
  let ascending = compare
  let[@inline] shift_right (local_ { global = t }) n = { global = Z.shift_right t n }
  let[@inline] shift_left (local_ { global = t }) n = { global = Z.shift_left t n }
  let[@inline] bit_not (local_ { global = x }) = { global = Z.lognot x }

  let[@inline] bit_xor (local_ { global = x }) (local_ { global = y }) =
    { global = Z.logxor x y }
  ;;

  let[@inline] bit_or (local_ { global = x }) (local_ { global = y }) =
    { global = Z.logor x y }
  ;;

  let[@inline] bit_and (local_ { global = x }) (local_ { global = y }) =
    { global = Z.logand x y }
  ;;

  let ( land ) = bit_and
  let ( lor ) = bit_or
  let ( lxor ) = bit_xor
  let lnot = bit_not
  let ( lsl ) = shift_left
  let ( asr ) = shift_right
  let[@inline] of_int x = { global = Z.of_int x }
  let[@inline] of_int32 (local_ x) = { global = Z.of_int32 x }
  let[@inline] of_int64 (local_ x) = { global = Z.of_int64 x }
  let[@inline] of_nativeint (local_ x) = { global = Z.of_nativeint x }
  let[@inline] of_float_unchecked (local_ x) = { global = Z.of_float x }
  let of_float = of_float_unchecked
  let of_int_exn = of_int
  let of_int32_exn = of_int32
  let of_local_int32_exn = of_int32
  let of_int64_exn = of_int64
  let of_local_int64_exn = of_int64
  let of_nativeint_exn = of_nativeint
  let of_local_nativeint_exn = of_nativeint
  let[@inline] to_int_exn { global = t } = Z.to_int t
  let[@inline] to_int32_exn (local_ { global = t }) = Z.to_int32 t
  let[@inline] to_int64_exn (local_ { global = t }) = Z.to_int64 t
  let[@inline] to_nativeint_exn (local_ { global = t }) = Z.to_nativeint t
  let[@inline] to_float (local_ { global = t }) = Z.to_float t
  let zero = { global = Z.zero }
  let one = { global = Z.one }
  let minus_one = { global = Z.minus_one }
  let to_int { global = t } = if Z.fits_int t then Some (Z.to_int t) else None
  let to_int32 { global = t } = if Z.fits_int32 t then Some (Z.to_int32 t) else None
  let to_int64 { global = t } = if Z.fits_int64 t then Some (Z.to_int64 t) else None
  let to_local_int32_exn = to_int32_exn
  let to_local_int64 = to_int64
  let to_local_int64_exn = to_int64_exn
  let to_local_nativeint_exn = to_nativeint_exn

  let to_nativeint { global = t } =
    if Z.fits_nativeint t then Some (Z.to_nativeint t) else None
  ;;

  let ( <> ) x y = not (equal x y)
  let incr (local_ cell) = cell := succ !cell
  let decr (local_ cell) = cell := pred !cell
  let pow (local_ { global = x }) (local_ y) = { global = Z.pow x (to_int_exn y) }
  let ( ** ) = pow
  let[@inline] popcount (local_ { global = t }) = Z.popcount t |> of_int
end

module Local_comparisons = struct
  let ( = ) = Unstable.equal__local
  let[@inline] ( <> ) x y = not (x = y)
  let[@inline] ( <= ) { global = x } { global = y } = Z.leq x y
  let[@inline] ( >= ) { global = x } { global = y } = Z.geq x y
  let[@inline] ( < ) { global = x } { global = y } = Z.lt x y
  let[@inline] ( > ) { global = x } { global = y } = Z.gt x y
end

module T_math = Int_math.Make (struct
    include Unstable
    include Local_comparisons
  end)

module T_conversions = Int_conversions.Make (Unstable)

module%template T_comparable_with_zero =
  Comparable.With_zero [@modality portable] (Unstable)

module%template T_identifiable =
Identifiable.Make [@mode local] [@modality portable] (struct
    let module_name = module_name

    include Unstable
  end)

(* Including in opposite order to shadow functorized bindings with direct bindings. *)
module O = struct
  include T_identifiable
  include T_comparable_with_zero
  include T_conversions
  include T_math
  include Unstable
end

include O

module Summable = struct
  type nonrec t = t

  let zero = zero
  let[@inline] ( + ) x y = x + y
  let[@inline] ( - ) x y = x - y
end

module Make_random (State : sig
  @@ portable
    type t

    val bits : t -> int
    val int : t -> int -> int
  end) : sig
  @@ portable
  val random : state:State.t -> t -> t
end = struct
  (* Uniform random generation of Bigint values.

     [random ~state range] chooses a [depth] and generates random values using
     [Random.State.bits state], called [1 lsl depth] times and concatenated. The
     preliminary result [n] therefore satisfies [0 <= n < 1 lsl (30 lsl depth)].

     In order for the random choice to be uniform between [0] and [range-1], there must
     exist [k > 0] such that [n < k * range <= 1 lsl (30 lsl depth)]. If so, [n % range]
     is returned. Otherwise the random choice process is repeated from scratch.

     The [depth] value is chosen so that repeating is uncommon (1 in 1,000 or less). *)

  let bits_at_depth ~depth = Int.shift_left 30 depth
  let range_at_depth ~depth = shift_left one (bits_at_depth ~depth)

  let rec choose_bit_depth_for_range_from ~range ~depth =
    if range_at_depth ~depth >= range
    then depth
    else choose_bit_depth_for_range_from ~range ~depth:(Int.succ depth)
  ;;

  let choose_bit_depth_for_range ~range = choose_bit_depth_for_range_from ~range ~depth:0

  let rec random_bigint_at_depth ~state ~depth =
    if Int.equal depth 0
    then of_int (State.bits state)
    else (
      let prev_depth = Int.pred depth in
      let prefix = random_bigint_at_depth ~state ~depth:prev_depth in
      let suffix = random_bigint_at_depth ~state ~depth:prev_depth in
      bit_or (shift_left prefix (bits_at_depth ~depth:prev_depth)) suffix)
  ;;

  let random_value_is_uniform_in_range ~range ~depth n =
    let k = range_at_depth ~depth / range in
    n < k * range
  ;;

  let rec large_random_at_depth ~state ~range ~depth =
    let result = random_bigint_at_depth ~state ~depth in
    if random_value_is_uniform_in_range ~range ~depth result
    then result % range
    else large_random_at_depth ~state ~range ~depth
  ;;

  let large_random ~state ~range =
    let tolerance_factor = of_int 1_000 in
    let depth = choose_bit_depth_for_range ~range:(range * tolerance_factor) in
    large_random_at_depth ~state ~range ~depth
  ;;

  let random ~state range =
    if range <= zero
    then
      failwithf "Bigint.random: argument %s <= 0" (to_string_hum range) ()
      (* Note that it's not safe to do [1 lsl 30] on a 32-bit machine (with 31-bit signed
         integers) *)
    else if range < shift_left one 30
    then of_int (State.int state (to_int_exn range))
    else large_random ~state ~range
  ;;
end

module Random_internal = Make_random (Random.State)

let random ?(state = Random.State.get_default ()) range =
  Random_internal.random ~state range
;;

module For_quickcheck : sig @@ portable
  include%template Quickcheckable.S_int [@mode portable] with type t := t

  val gen_negative : t Quickcheck.Generator.t
  val gen_positive : t Quickcheck.Generator.t
end = struct
  module Generator = Base_quickcheck.Generator
  open Generator.Portable.Let_syntax

  module Uniform = Make_random (struct
      type t = Splittable_random.t

      let int t range = Splittable_random.int t ~lo:0 ~hi:(Int.pred range)
      let bits t = int t (Int.shift_left 1 30)
    end)

  let random_uniform ~state lo hi = lo + Uniform.random ~state (succ (hi - lo))

  let%template gen_uniform_incl lower_bound upper_bound =
    if lower_bound > upper_bound
    then
      raise_s
        [%message
          "Bigint.gen_uniform_incl: bounds are crossed"
            (lower_bound : t)
            (upper_bound : t)];
    (Generator.create [@mode portable]) (fun ~size:_ ~random:state ->
      random_uniform ~state lower_bound upper_bound)
  ;;

  let%template gen_incl lower_bound upper_bound =
    (Generator.weighted_union [@mode portable])
      [ 0.05, (Generator.return [@mode portable]) lower_bound
      ; 0.05, (Generator.return [@mode portable]) upper_bound
      ; 0.9, gen_uniform_incl lower_bound upper_bound
      ]
  ;;

  let min_represented_by_n_bits n =
    if Int.equal n 0 then zero else shift_left one (Int.pred n)
  ;;

  let max_represented_by_n_bits n = pred (shift_left one n)

  let gen_log_uniform_incl lower_bound upper_bound =
    if lower_bound < zero || lower_bound > upper_bound
    then
      raise_s
        [%message
          "Bigint.gen_log_incl: invalid bounds" (lower_bound : t) (upper_bound : t)];
    let min_bits = Z.numbits lower_bound.global in
    let max_bits = Z.numbits upper_bound.global in
    let%bind bits = Generator.int_uniform_inclusive min_bits max_bits in
    gen_uniform_incl
      (max lower_bound (min_represented_by_n_bits bits))
      (min upper_bound (max_represented_by_n_bits bits))
  ;;

  let%template gen_log_incl lower_bound upper_bound =
    (Generator.weighted_union [@mode portable])
      [ 0.05, (Generator.return [@mode portable]) lower_bound
      ; 0.05, (Generator.return [@mode portable]) upper_bound
      ; 0.9, gen_log_uniform_incl lower_bound upper_bound
      ]
  ;;

  let%template gen_positive =
    let%bind extra_bytes = Generator.size in
    let num_bytes = Int.succ extra_bytes in
    let num_bits = Int.( * ) num_bytes 8 in
    gen_log_uniform_incl one (pred (shift_left one num_bits))
  ;;

  let%template gen_negative = (Generator.map [@mode portable]) gen_positive ~f:neg

  let%template quickcheck_generator =
    (Generator.weighted_union [@mode portable])
      [ 0.45, gen_positive
      ; 0.1, (Generator.return [@mode portable]) zero
      ; 0.45, gen_negative
      ]
  ;;

  let%template quickcheck_observer =
    (Quickcheck.Observer.create [@mode portable]) (fun t ~size:_ ~hash ->
      hash_fold_t hash t)
  ;;

  let quickcheck_shrinker = Quickcheck.Shrinker.empty ()
end

include For_quickcheck

module Hex = struct
  type nonrec t = t [@@deriving bin_io ~localize, typerep]

  module M = Base.Int_conversions.Make_hex (struct
      type nonrec t = t [@@deriving hash, compare ~localize]

      let to_string { global = i } = Z.format "%x" i
      let of_hex_string str = Z.of_string_base 16 str

      let of_string str =
        of_string_base str ~name:"Hex.of_string" ~of_string:of_hex_string
      ;;

      let ( < ) = Local_comparisons.( < )
      let neg = neg
      let zero = zero
      let module_name = module_name ^ ".Hex"
    end)

  include (
    M.Hex :
      module type of struct
        include M.Hex
      end
      with type t := t)
end

module Binary = struct
  type nonrec t = t [@@deriving bin_io ~localize, compare ~localize, hash, typerep]

  let to_string { global = t } = Z.format "%#b" t
  let chars_per_delimiter = 4

  let to_string_hum ?(delimiter = '_') { global = t } =
    let input = Z.format "%b" t in
    "0b" ^ Int_conversions.insert_delimiter_every input ~delimiter ~chars_per_delimiter
  ;;

  let sexp_of_t t : Sexp.t = Atom (to_string t)

  module Hum = struct
    let to_string = to_string_hum
  end
end

include T
