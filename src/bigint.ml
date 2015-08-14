open Core_kernel.Std

module Z = Zarith_1_3.Z
;;

type t = Z.t with typerep(abstract)
;;

module Stable = struct

  module V1 = struct

    module T0 = struct

      type nonrec t = t
      ;;

      let module_name = "Bignum.Std.Bigint"
      ;;

      let to_string = Z.to_string
      ;;

      let rec is_integer_suffix s i ~len ~char_is_digit =
        if i < len
        then
          let c = String.get s i in
          if char_is_digit c || Char.equal c '_'
          then is_integer_suffix s (i+1) ~len ~char_is_digit
          else false
        else true
      ;;

      let is_integer_string s ~char_is_digit =
        let len = String.length s in
        if 0 < len
        then
          let i = if Char.equal (String.get s 0) '-' then 1 else 0 in
          if i < len
          then
            if char_is_digit (String.get s i)
            then is_integer_suffix s (i+1) ~len ~char_is_digit
            else false
          else false
        else false
      ;;

      let of_string_base str ~name ~of_string_no_underscores ~char_is_digit =
        try of_string_no_underscores str with _ ->
          if is_integer_string str ~char_is_digit
          then of_string_no_underscores (String.filter str ~f:(fun c -> c <> '_'))
          else failwithf "%s.%s: invalid argument %S" name module_name str ()
      ;;

      let of_string str =
        of_string_base str
          ~name:"of_string"
          ~of_string_no_underscores:Z.of_string
          ~char_is_digit:Char.is_digit
      ;;

      let compare = Z.compare
      ;;

      module Binable = struct
        type t = Zero | Pos of string | Neg of string with bin_io
      end
      ;;

      let to_binable t =
        let s = Z.sign t in
        if s > 0 then Binable.Pos (Z.to_bits t) else
        if s < 0 then Binable.Neg (Z.to_bits t) else
          Binable.Zero
      ;;

      let of_binable = function
        | Binable.Zero -> Z.zero
        | Binable.Pos bits -> Z.of_bits bits
        | Binable.Neg bits -> Z.of_bits bits |> Z.neg
      ;;

    end

    include Sexpable.Of_stringable( T0 )
    include Binable.Of_binable (T0.Binable) (T0)
    include T0
    ;;

  end

  module Current = V1
  ;;

end
;;

module T = struct

  include Stable.Current
  ;;

  let of_zarith_bigint t = t
  let to_zarith_bigint t = t
  ;;

  let (/%) x y =
    if Z.sign y >= 0
    then Z.ediv x y
    else
      failwithf "%s.(%s /%% %s) : divisor must be positive"
        module_name
        (to_string x)
        (to_string y)
        ()
  ;;

  let (%) x y =
    if Z.sign y >= 0
    then Z.erem x y
    else
      failwithf "%s.(%s %% %s) : divisor must be positive"
        module_name
        (to_string x)
        (to_string y)
        ()
  ;;

  let hash = Z.hash
  let compare = Z.compare
  ;;

  let ( - ) = Z.( - )
  let ( + ) = Z.( + )
  let ( * ) = Z.( * )
  let ( / ) = Z.( / )
  ;;

  let rem = Z.rem
  ;;

  let (~-) = Z.(~-)
  let neg = Z.neg
  let abs = Z.abs
  let succ = Z.succ
  let pred = Z.pred
  ;;

  let equal = Z.equal
  let (=) = Z.equal
  let (<) = Z.lt
  let (>) = Z.gt
  let (<=) = Z.leq
  let (>=) = Z.geq
  let max = Z.max
  let min = Z.min
  let ascending = compare
  ;;

  let shift_right = Z.shift_right
  let shift_left = Z.shift_left
  let bit_not = Z.lognot
  let bit_xor = Z.logxor
  let bit_or = Z.logor
  let bit_and = Z.logand
  ;;

  let of_int = Z.of_int
  let of_int32 = Z.of_int32
  let of_int64 = Z.of_int64
  let of_nativeint = Z.of_nativeint
  let of_float = Z.of_float
  ;;

  let of_int_exn = of_int
  let of_int32_exn = of_int32
  let of_int64_exn = of_int64
  let of_nativeint_exn = of_nativeint
  ;;

  let to_int_exn = Z.to_int
  let to_int32_exn = Z.to_int32
  let to_int64_exn = Z.to_int64
  let to_nativeint_exn = Z.to_nativeint
  let to_float = Z.to_float
  ;;

  let zero = Z.zero
  let one = Z.one
  let minus_one = Z.minus_one
  ;;

  let to_int t = if Z.fits_int t then Some (Z.to_int t) else None
  let to_int32 t = if Z.fits_int32 t then Some (Z.to_int32 t) else None
  let to_int64 t = if Z.fits_int64 t then Some (Z.to_int64 t) else None
  let to_nativeint t = if Z.fits_nativeint t then Some (Z.to_nativeint t) else None
  ;;

  let (<>) x y = not (equal x y)
  ;;

  let incr cell = cell := succ !cell
  let decr cell = cell := pred !cell
  ;;

  let pow x y = Z.pow x (to_int_exn y)
  ;;

  let num_bits            = `Bigint_is_unbounded
  let max_value           = `Bigint_is_unbounded
  let min_value           = `Bigint_is_unbounded
  let shift_right_logical = `Bigint_is_unbounded
  ;;

end
;;

module T_math = Core_kernel.Int_math.Make( T )
module T_conversions = Core_kernel.Int_conversions.Make( T )
module T_comparable_with_zero = Comparable.Validate_with_zero( T )
module T_identifiable = Identifiable.Make( T )
;;

(* Including in opposite order to shadow functorized bindings with direct bindings. *)
module O = struct
  include T_identifiable
  include T_comparable_with_zero
  include T_conversions
  include T_math
  include T
end
;;

include (O : module type of O with type t := t)
;;

let is_even x = equal zero (bit_and x one)

let midpoint x y =
  let midpoint = shift_right (x + y) 1 in
  if Bool.equal (is_even x) (is_even y)
  then `Single midpoint
  else `Double (midpoint, succ midpoint)

let rec gen_between_inclusive ~lower_bound ~upper_bound =
  let open Quickcheck.Generator in
  if lower_bound > upper_bound
  then failure
  else if equal lower_bound upper_bound
  then singleton lower_bound
  else if equal (succ lower_bound) upper_bound
  then doubleton lower_bound upper_bound
  else
    match midpoint lower_bound upper_bound with
    | `Single midpoint ->
      union
        [ singleton lower_bound
        ; of_fun (fun () ->
            gen_between_inclusive
              ~lower_bound:(succ lower_bound)
              ~upper_bound:(pred midpoint))
        ; singleton midpoint
        ; of_fun (fun () ->
            gen_between_inclusive
              ~lower_bound:(succ midpoint)
              ~upper_bound:(pred upper_bound))
        ; singleton upper_bound
        ]
    | `Double (lower_mid, upper_mid) ->
      union
        [ singleton lower_bound
        ; of_fun (fun () ->
            gen_between_inclusive
              ~lower_bound:(succ lower_bound)
              ~upper_bound:(pred lower_mid))
        ; singleton lower_mid
        ; singleton upper_mid
        ; of_fun (fun () ->
            gen_between_inclusive
              ~lower_bound:(succ upper_mid)
              ~upper_bound:(pred upper_bound))
        ; singleton upper_bound
        ]

let gen_positive =
  let open Quickcheck.Generator in
  let rec loop exponent =
    let lower_bound = shift_left one exponent in
    let upper_bound = pred (shift_left lower_bound 1) in
    union
      [ gen_between_inclusive ~lower_bound ~upper_bound
      ; of_fun (fun () -> loop (Int.succ exponent))
      ]
  in
  loop 0

let gen_negative =
  let open Quickcheck.Generator in
  gen_positive
  >>| neg

let gen =
  let open Quickcheck.Generator in
  union
    [ singleton zero
    ; gen_positive
    ; gen_negative
    ]

let gen_between ~lower_bound ~upper_bound =
  let open Quickcheck.Generator in
  match lower_bound, upper_bound with
  | Unbounded, Unbounded -> gen
  | Unbounded, Excl upper ->
    map gen_negative ~f:(fun x -> x + upper)
  | Unbounded, Incl upper ->
    let upper = succ upper in
    map gen_negative ~f:(fun x -> x + upper)
  | Excl lower, Unbounded ->
    map gen_positive ~f:(fun x -> x + lower)
  | Incl lower, Unbounded ->
    let lower = pred lower in
    map gen_positive ~f:(fun x -> x + lower)
  | Incl lower_bound, Incl upper_bound ->
    gen_between_inclusive ~lower_bound ~upper_bound
  | Incl lower_bound, Excl upper_bound ->
    let upper_bound = pred upper_bound in
    gen_between_inclusive ~lower_bound ~upper_bound
  | Excl lower_bound, Incl upper_bound ->
    let lower_bound = succ lower_bound in
    gen_between_inclusive ~lower_bound ~upper_bound
  | Excl lower_bound, Excl upper_bound ->
    let lower_bound = succ lower_bound in
    let upper_bound = pred upper_bound in
    gen_between_inclusive ~lower_bound ~upper_bound

let rec obs_upper_bits ~lower_bound ~upper_bound =
  let open Quickcheck.Observer in
  if lower_bound > upper_bound
  then failwith "Observer.obs_between_inclusive: lower_bound > upper_bound"
  else if lower_bound = upper_bound then singleton ()
  else
    let midpoint = shift_right (lower_bound + upper_bound) 1 in
    of_predicate ~f:(fun x -> x <= midpoint)
      ~f_sexp:(fun () -> <:sexp_of< [`le of t] >> (`le midpoint))
      (of_fun (fun () -> obs_lower_bits ~lower_bound ~upper_bound:midpoint))
      (of_fun (fun () -> obs_lower_bits ~lower_bound:(midpoint+one) ~upper_bound))

and obs_lower_bits ~lower_bound ~upper_bound =
  let open Quickcheck.Observer in
  if lower_bound > upper_bound
  then failwith "Observer.obs_between_inclusive: lower_bound > upper_bound"
  else if lower_bound = upper_bound then singleton ()
  else
    tuple2 bool
      (obs_upper_bits
         ~lower_bound:(shift_right lower_bound 1)
         ~upper_bound:(shift_right upper_bound 1))
    |> unmap ~f:(fun x -> is_even x, shift_right x 1)
         ~f_sexp:(fun () -> Sexp.Atom "is_even_and_all_but_least_significant_bit")

let obs_between_inclusive = obs_lower_bits

let obs_positive =
  let open Quickcheck.Observer in
  let rec loop bits =
    let lower_bound = shift_left one (Int.pred bits) in
    let upper_bound = pred (shift_left one bits) in
    of_predicate ~f:(fun x -> x <= upper_bound)
      ~f_sexp:(fun () -> <:sexp_of< [`le of t] >> (`le upper_bound))
      (obs_between_inclusive ~lower_bound ~upper_bound)
      (of_fun (fun () -> loop (Int.succ bits)))
  in
  loop 1

let obs_negative =
  let open Quickcheck.Observer in
  unmap obs_positive ~f:neg ~f_sexp:(fun () -> Sexp.Atom "Bigint.neg")

let obs =
  let open Quickcheck.Observer in
  comparison ~compare
    ~eq:zero
    ~lt:obs_negative
    ~gt:obs_positive
    ~compare_sexp:(fun () -> Sexp.Atom "Bigint.compare")
    ~sexp_of_eq:<:sexp_of< t >>

module Random_internal : sig
  val random :  ?state:Random.State.t -> t -> t
end = struct

  (* Uniform random generation of Bigint values.

     [random ~state range] chooses a [depth] and generates random values using
     [Random.State.bits state], called [1 lsl depth] times and concatenated.  The
     preliminary result [n] therefore satisfies [0 <= n < 1 lsl (30 lsl depth)].

     In order for the random choice to be uniform between [0] and [range-1], there must
     exist [k > 0] such that [n < k * range <= 1 lsl (30 lsl depth)].  If so, [n % range]
     is returned.  Otherwise the random choice process is repeated from scratch.

     The [depth] value is chosen so that repeating is uncommon (1 in 1,000 or less). *)

  let bits_at_depth ~depth = 30 lsl depth

  let range_at_depth ~depth = shift_left one (bits_at_depth ~depth)

  let rec choose_bit_depth_for_range_from ~range ~depth =
    if range_at_depth ~depth >= range
    then depth
    else choose_bit_depth_for_range_from ~range ~depth:(Int.succ depth)
  ;;

  let choose_bit_depth_for_range ~range =
    choose_bit_depth_for_range_from ~range ~depth:0
  ;;

  let rec random_bigint_at_depth ~state ~depth =
    if Int.equal depth 0
    then of_int (Random.State.bits state)
    else
      let prev_depth = Int.pred depth in
      let prefix = random_bigint_at_depth ~state ~depth:prev_depth in
      let suffix = random_bigint_at_depth ~state ~depth:prev_depth in
      bit_or
        (shift_left prefix (bits_at_depth ~depth:prev_depth))
        suffix
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

  let random ?(state = Random.State.default) range =
    if range <= zero
    then failwithf "Bigint.random: argument %s <= 0" (to_string_hum range) ()
    (* Note that it's not safe to do [1 lsl 30] on a 32-bit machine (with 31-bit signed
       integers) *)
    else if range < shift_left one 30
    then of_int (Random.State.int state (to_int_exn range))
    else large_random ~state ~range
  ;;
end

let random = Random_internal.random

TEST_UNIT "random" =
  let state = Random.State.make [| 1 ; 2 ; 3 |] in
  let range = shift_left one 100 in
  let seen = Hash_set.create () in
  for _i = 1 to 100_000 do
    let t = random ~state range in
    if t < zero || t >= range then failwith "random result out of bounds";
    Core_kernel.Std.Hash_set.strict_add_exn seen t
  done
;;

include Core_kernel.Int_conversions.Make_hex(struct

  type nonrec t = t with bin_io, compare, typerep
  ;;

  let to_string i = Z.format "%x" i
  ;;

  let char_is_hex_digit = function
    | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
    | _ -> false
  ;;

  let of_hex_string_no_underscores str =
    Z.of_string_base 16 str
  ;;

  let of_string str =
    of_string_base str
      ~name:"Hex.of_string"
      ~char_is_digit:char_is_hex_digit
      ~of_string_no_underscores:of_hex_string_no_underscores
  ;;

  let (<) = (<)
  let neg = neg
  let zero = zero

  let module_name = module_name ^ ".Hex"

end)
;;


TEST_MODULE "stable bin_io" = struct

  let array =
    Array.init 10 ~f:(fun i ->
      pow (of_int 1_000_000_000) (of_int i))
  ;;

  let size_of_buf = 1024

  let buf = Bigstring.create size_of_buf

  TEST_UNIT "round-trip" =
    for pos = 0 to 20 do
      Array.iter array ~f:(fun t ->
        let size_of_t = Stable.V1.bin_size_t t in
        assert Int.(size_of_t + pos <= size_of_buf);
        let new_pos = Stable.V1.bin_writer_t.Bin_prot.Type_class.write buf ~pos t in
        let pos_ref = ref pos in
        let t1 = Stable.V1.bin_reader_t.Bin_prot.Type_class.read buf ~pos_ref in
        <:test_result< Stable.V1.t >> t1 ~expect:t;
        <:test_result< int >> !pos_ref ~expect:new_pos;
      )
    done
  ;;
end

TEST_MODULE "vs Int" = struct

  TEST_UNIT "constants" =
    <:test_eq<int>> Int.zero      (to_int_exn zero);
    <:test_eq<int>> Int.one       (to_int_exn one);
    <:test_eq<int>> Int.minus_one (to_int_exn minus_one)
  ;;

  TEST_UNIT "unary" =
    let nums =
      [ -1001001001 ; -1001001 ; -1001 ; -1 ; 0 ; 1 ; 1234 ; 1234567 ; 123456789 ]
    in
    let ops =
      [ Int.( ~- ) , ( ~- ), "( ~- )"
      ; Int.neg    , neg    , "neg"
      ; Int.abs    , abs    , "abs"
      ; Int.succ   , succ   , "succ"
      ; Int.pred   , pred   , "pred"
      ; Int.bit_not, bit_not, "bit_not"
      ]
    in
    List.iter ops ~f:(fun (int_op, big_op, op_str) ->
      List.iter nums ~f:(fun int_x ->
        let expect = Option.try_with (fun () -> int_op int_x) in
        let big_x = of_int_exn int_x in
        let big_actual = Option.try_with (fun () -> big_op big_x) in
        let int_actual = Option.map big_actual ~f:to_int_exn in
        <:test_result<int option>>
          ~message:(sprintf "Bigint does not match [Int.%s %d]" op_str int_x)
          ~expect
          int_actual))
  ;;

  TEST_UNIT "binops" =
    let nums =
      [ -10101 ; -101 ; -1 ; 0 ; 1 ; 123 ; 12345 ]
    in
    let wrap_round f x y = f x ~to_multiple_of:y in
    let wrap_compare f x y = of_int_exn (f x y) in
    let ops =
      [ Int.( + ) , ( + ) , "( + )"
      ; Int.( - ) , ( - ) , "( - )"
      ; Int.( * ) , ( * ) , "( * )"
      ; Int.( / ) , ( / ) , "( / )"
      ; Int.rem   , rem   , "rem"
      ; Int.( /% ), ( /% ), "( /% )"
      ; Int.( % ) , ( % ) , "( % )"
      ; Int.bit_and, bit_and, "bit_and"
      ; Int.bit_or , bit_or , "bit_or"
      ; Int.bit_xor, bit_xor, "bit_xor"
      ; Int.compare, wrap_compare compare, "compare"
      ; wrap_round Int.round_down    , wrap_round round_down    , "round_down"
      ; wrap_round Int.round_up      , wrap_round round_up      , "round_up"
      ; wrap_round Int.round_nearest , wrap_round round_nearest , "round_nearest"
      ; ( wrap_round Int.round_towards_zero
        , wrap_round round_towards_zero
        , "round_towards_zero" )
      ]
    in
    List.iter ops ~f:(fun (int_op, big_op, op_str) ->
      List.iter nums ~f:(fun int_x ->
        List.iter nums ~f:(fun int_y ->
          let expect = Option.try_with (fun () -> int_op int_x int_y) in
          let big_x = of_int_exn int_x in
          let big_y = of_int_exn int_y in
          let big_actual = Option.try_with (fun () -> big_op big_x big_y) in
          let int_actual = Option.map big_actual ~f:to_int_exn in
          <:test_result<int option>>
            ~message:(sprintf "Bigint does not match [Int.%s %d %d]" op_str int_x int_y)
            ~expect
            int_actual)))
  ;;

  TEST_UNIT "comparisons" =
    let nums =
      [ -1001001001 ; -1001001 ; -1001 ; -1 ; 0 ; 1 ; 1234 ; 1234567 ; 123456789 ]
    in
    let ops =
      [ Int.( <> ), ( <> ), "( <> )"
      ; Int.( <= ), ( <= ), "( <= )"
      ; Int.( >= ), ( >= ), "( >= )"
      ; Int.( < ) , ( < ) , "( < )"
      ; Int.( > ) , ( > ) , "( > )"
      ; Int.( = ) , ( = ) , "( = )"
      ; Int.equal, equal, "equal"
      ]
    in
    List.iter ops ~f:(fun (int_op, big_op, op_str) ->
      List.iter nums ~f:(fun int_x ->
        List.iter nums ~f:(fun int_y ->
          let expect = int_op int_x int_y in
          let big_x = of_int_exn int_x in
          let big_y = of_int_exn int_y in
          let actual = big_op big_x big_y in
          <:test_result<bool>>
            ~message:(sprintf "Bigint does not match [Int.%s %d %d]" op_str int_x int_y)
            ~expect
            actual)))
  ;;

  TEST_UNIT "shift" =
    let nums =
      [ -10101 ; -101 ; -1 ; 0 ; 1 ; 123 ; 12345 ]
    in
    let ops =
      [ Int.shift_left , shift_left , "shift_left"
      ; Int.shift_right, shift_right, "shift_right"
      ]
    in
    List.iter ops ~f:(fun (int_op, big_op, op_str) ->
      List.iter nums ~f:(fun int_x ->
        for int_y = 0 to 15 do
          let expect = Option.try_with (fun () -> int_op int_x int_y) in
          let big_x = of_int_exn int_x in
          let big_actual = Option.try_with (fun () -> big_op big_x int_y) in
          let int_actual = Option.map big_actual ~f:to_int_exn in
          <:test_result<int option>>
            ~message:(sprintf "Bigint does not match [Int.%s %d %d]" op_str int_x int_y)
            ~expect
            int_actual
        done))
  ;;

  TEST_UNIT "pow" =
    let bases = [ -101 ; -11 ; -1 ; 0 ; 1 ; 12 ; 123 ] in
    List.iter bases ~f:(fun base ->
      for expt = -4 to 4 do
        let expect = Option.try_with (fun () -> Int.pow base expt) in
        let big_base = of_int_exn base in
        let big_expt = of_int_exn expt in
        let big_actual = Option.try_with (fun () -> pow big_base big_expt) in
        let int_actual = Option.map big_actual ~f:to_int_exn in
        <:test_result<int option>>
          ~message:(sprintf "Bigint does not match [Int.pow %d %d]" base expt)
          ~expect
          int_actual
      done)
  ;;

  TEST_UNIT "huge" =
    let huge_val = pow (of_int_exn 1001) (of_int_exn 10) in
    let huge_str = "1010045120210252210120045010001" in
    let huge_hum = "1_010_045_120_210_252_210_120_045_010_001" in
    let huge_hex = "0xcbfa1bdc2045351f4de129c51" in
    let huge_hex_hum = "0xc_bfa1_bdc2_0453_51f4_de12_9c51" in
    let huge_hex_caps = String.uppercase huge_hex_hum in
    let huge_sexp = Sexp.Atom huge_str in
    let huge_hex_sexp = Sexp.Atom huge_hex in
    <:test_result<int option>>
      (Option.try_with (fun () -> to_int_exn huge_val))
      ~expect:None;
    <:test_result< string >> (to_string huge_val)          ~expect:huge_str;
    <:test_result< string >> (to_string_hum huge_val)      ~expect:huge_hum;
    <:test_result< Sexp.t >> (sexp_of_t huge_val)          ~expect:huge_sexp;
    <:test_result< t >>      (of_string huge_str)          ~expect:huge_val;
    <:test_result< t >>      (of_string huge_hum)          ~expect:huge_val;
    <:test_result< t >>      (t_of_sexp huge_sexp)         ~expect:huge_val;
    <:test_result< string >> (Hex.to_string huge_val)      ~expect:huge_hex;
    <:test_result< string >> (Hex.to_string_hum huge_val)  ~expect:huge_hex_hum;
    <:test_result< Sexp.t >> (Hex.sexp_of_t huge_val)      ~expect:huge_hex_sexp;
    <:test_result< t >>      (Hex.of_string huge_hex)      ~expect:huge_val;
    <:test_result< t >>      (Hex.of_string huge_hex_hum)  ~expect:huge_val;
    <:test_result< t >>      (Hex.of_string huge_hex_caps) ~expect:huge_val;
    <:test_result< t >>      (Hex.t_of_sexp huge_hex_sexp) ~expect:huge_val;
  ;;

end
;;