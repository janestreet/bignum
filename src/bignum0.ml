open Core_kernel.Std

module Stable = struct
  module V1 = struct
    module Zarith = Zarith_1_3
    module Z = Zarith.Z

    include Zarith.Q

    let num t = of_bigint t.num
    let den t = of_bigint t.den

    let one      = of_int 1
    let ten      = of_int 10
    let hundred  = of_int 100
    let thousand = of_int 1_000
    let million  = of_int 1_000_000
    let billion  = of_int 1_000_000_000
    let trillion = million * million

    let tenth      = one / ten
    let hundredth  = one / hundred
    let thousandth = one / thousand
    let millionth  = one / million
    let billionth  = one / billion
    let trillionth = one / trillion

    let nan          = zero      / zero
    let infinity     = one       / zero
    let neg_infinity = minus_one / zero

    let to_rational_string = to_string
    let of_rational_string s = of_string s

    let to_float_string =
      let (-) = Int.(-) in
      let (+) = Int.(+) in
      let decimal_mover = of_int 1_000_000_000 in
      let shift_len     = String.length (to_string decimal_mover) - 1 in
      (fun t ->
         let neg     = t < zero in
         let shifted = mul (abs t) decimal_mover in
         let num,den = shifted.num,shifted.den in
         let s       = Z.to_string (Z.div num den) in
         let rec dec_end_pos pos count =
           if pos < 0 || count = shift_len then None
           else begin
             if Char.(=) s.[pos] '0' then dec_end_pos (pos - 1) (count + 1)
             else (Some pos)
           end
         in
         let len = String.length s in
         let int_part,dec_part =
           match dec_end_pos (String.length s - 1) 0 with
           | None ->
             let int_part =
               if len > shift_len then String.sub s ~pos:0 ~len:(len - shift_len)
               else ""
             in
             int_part, ""
           | Some end_pos ->
             let int_len  = if len > shift_len then len - shift_len else 0 in
             let int_part = if int_len > 0 then String.sub s ~pos:0 ~len:int_len else "" in
             let dec_pad  =
               if len >= shift_len then "" else String.make (shift_len - len) '0'
             in
             let dec_part = dec_pad ^ String.sub s ~pos:int_len ~len:(end_pos - int_len + 1) in
             int_part,dec_part
         in
         match neg,int_part,dec_part with
         | _,"",""    -> "0"
         | true,"",_  -> "-0." ^ dec_part
         | false,"",_ -> "0." ^ dec_part
         | true,_,""  -> "-" ^ int_part
         | false,_,"" -> int_part
         | true,_,_   -> "-" ^ int_part ^ "." ^ dec_part
         | false,_,_  -> int_part ^ "." ^ dec_part)
    ;;

    let to_string t =
      if Z.(t.den = zero) then
        if Z.(t.num > zero) then "inf" else
        if Z.(t.num < zero) then "-inf" else
          "nan"
      else
        to_float_string t

    let of_float_string s =
      let fail () = failwithf "unable to parse %S as Bignum.t" s () in
      if String.is_empty s then fail ();
      let is_negative, skip_sign =
        match s.[0] with
        | '-' -> true , 1
        | '+' -> false, 1
        | _   -> false, 0
      in
      let seen_digit = ref false in
      let seen_dot = ref false in
      (* do all the validation upfront *)
      for i = skip_sign to Int.O.(String.length s - 1) do
        if Char.is_digit s.[i]
        then seen_digit := true
        else if Char.equal s.[i] '.'
        then begin
          if !seen_dot
          then fail () (* can't have two dots *)
          else seen_dot := true
        end
        else fail () (* can't have anything not a digit or dot *)
      done;
      if not !seen_digit then fail ();
      (* okay, now there are no more error cases *)
      if not !seen_dot
      then of_bigint (Z.of_string s)
      else begin
        let int_part, frac_part = String.lsplit2_exn s ~on:'.' in
        let frac_len  = String.length frac_part in
        let den       = Z.pow (Z.of_int 10) frac_len in
        let int_part  = Z.(of_string int_part * den) in
        let frac_part = Z.of_string frac_part in
        let num = if is_negative
          then Z.sub int_part frac_part
          else Z.add int_part frac_part
        in
        make num den
      end
    ;;

    let of_string s =
      try
        let s =
          if String.contains s '_'
          then String.concat ~sep:"" (String.split ~on:'_' s)
          else s
        in
        let s = String.lowercase s in (* 'E' -> 'e' *)
        match s with
        | "nan" | "+nan" | "-nan" -> nan
        | "inf" | "+inf"          -> infinity
        | "-inf"                  -> neg_infinity
        | _                       ->
          if String.contains s '/' then of_rational_string s
          else if String.contains s 'e' then
            begin match String.lsplit2 ~on:'e' s with
            | None -> assert false
            | Some (float_part,power) ->
              let base   = of_float_string float_part in
              let power  = Int.of_string power in
              let power' = Z.pow (Z.of_int 10) (Int.abs power) in
              let power' =
                if Int.(>) power 0 then make power' Z.one
                else make Z.one power'
              in
              mul base power'
            end
          else of_float_string s
      with
      | e -> Exn.reraise e "Bignum.of_string"
    ;;

    TEST_UNIT "of_string parse failures" =
      List.iter
        [""; "hello"; "123x"; "."; "-."; "1.2.3"; "+-1"; "--1"; "-+1"; "++1"]
        ~f:(<:test_pred< string >> (fun s ->
          let doesn't_parse_with f =
            try ignore (f s); false with _ -> true
          in
          doesn't_parse_with of_float_string && doesn't_parse_with Float.of_string))
    ;;

    TEST = to_string (of_string "1.23456789") = "1.23456789"

    TEST = to_string (of_int 7 / of_int 9)     = "0.777777777"
    TEST = to_string (of_int (-7) / of_int 9)  = "-0.777777777"
    TEST = (of_float 766.46249999999997726) <> (of_float 766.462499999999864)

    TEST = of_string (to_string nan)          = nan
    TEST = of_string (to_string infinity)     = infinity
    TEST = of_string (to_string neg_infinity) = neg_infinity

    let sexp_of_t t =
      try
        if Z.(t.den = zero) then
          if Z.(t.num > zero) then Sexp.Atom "inf" else
          if Z.(t.num < zero) then Sexp.Atom "-inf" else
            Sexp.Atom "nan"
        else
          let float_string    = to_float_string t in
          let of_float_string = of_float_string float_string in
          if equal of_float_string t then Sexp.Atom float_string
          else begin
            let diff = sub t of_float_string in
            Sexp.List [
              Sexp.Atom float_string;
              Sexp.Atom "+";
              Sexp.Atom (to_rational_string diff) ]
          end
      with
      | e -> Exn.reraise e "Bignum.sexp_of_t"
    ;;

    TEST = Sexp.equal
             (sexp_of_t (of_int 1 / of_int 3))
             Sexp.O.(List [Atom "0.333333333"; Atom "+"; Atom "1/3000000000"])

    (* these are down here instead of with of_string
       because <:test_result< t >> uses sexp_of_t *)
    TEST_UNIT "of_string matches Float.of_string" =
      let as_float s =
        <:test_result< t >>
          ~expect:(of_float (Float.of_string s))
          (of_string s);
      in
      List.iter
        (* All representable exactly as floats *)
        [ "0"; ".0"; "0."; "00"
        ; "1"; ".5"; "1."; "01"
        ; "0.25"; "0.0625"; ".0625"; "01.0625"
        ; "1.375"; "1.75"; "99.9375"
        ; "1.2e5"; "1.2E5"; "0.5e0"; "125e-3"
        ] ~f:(fun s ->
          as_float s;
          as_float ("+" ^ s);
          as_float ("-" ^ s))

    let t_of_sexp s =
      match s with
      | Sexp.Atom s -> of_string s
      | Sexp.List [Sexp.Atom float_part; Sexp.Atom "+"; Sexp.Atom rational_part] ->
        let t1 = of_float_string float_part in
        let t2 = of_rational_string rational_part in
        add t1 t2
      | Sexp.List _ -> of_sexp_error "expected Atom or List [float; \"+\"; remainder]" s
    ;;

    TEST = t_of_sexp (sexp_of_t nan)          = nan
    TEST = t_of_sexp (sexp_of_t infinity)     = infinity
    TEST = t_of_sexp (sexp_of_t neg_infinity) = neg_infinity

    include Binable.Of_binable (String) (struct
        type nonrec t  = t

        let to_binable t = Zarith.Q.to_string t
        let of_binable s = Zarith.Q.of_string s

        TEST = of_binable (to_binable nan)          = nan
        TEST = of_binable (to_binable infinity)     = infinity
        TEST = of_binable (to_binable neg_infinity) = neg_infinity

      end)
  end
  module V2 = struct
    include V1
    (* The V2 serialized representation makes use of special case to
       achieve better compression AND less overhead when serialising /
       deserialising.

       It is written to go via an intermediate type.  However to gain
       additional speed during deserialisation, we provide a handexpanded
       read function that avoids the unnecessary allocation of the
       intermediate type. To do so the two types below must be kept in
       sync (including order of constructors) -- this is enforced by a
       unit test below. *)
    module Tag = struct
      type t =
        | Zero
        | Int
        | Over_10
        | Over_100
        | Over_1_000
        | Over_10_000
        | Over_100_000
        | Over_1_000_000
        | Over_10_000_000
        | Over_100_000_000
        | Over_int
        | Other
      with bin_io, variants
    end

    module Bin_rep = struct
      type t =
        | Zero
        | Int of int
        | Over_10 of int
        | Over_100 of int
        | Over_1_000 of int
        | Over_10_000 of int
        | Over_100_000 of int
        | Over_1_000_000 of int
        | Over_10_000_000 of int
        | Over_100_000_000 of int
        | Over_int of int * int
        | Other of V1.t
      with bin_io, variants
    end
    include Binable.Of_binable (Bin_rep) (struct
        type t = V1.t

        TEST "tag/binable constructors in sync" =
          List.for_all2_exn Tag.Variants.descriptions Bin_rep.Variants.descriptions
            ~f:(fun (tag_name, _) (bin_name, _) -> String.equal tag_name bin_name)

        (* To prevent a silent overflow that would result in a wrong result,
           we only optimise after having checked that the numerator will still fit in an int
           after having been multiplied by (i / d).*)
        (* pre condition: i > 0, d > 0 and d divides i *)
        let check_overflow f ~n ~d i =
          (* Let p = i / d. p is an integer (cf pre condition). We have i = p.d.
             n <= Max / i * d = Max / p.d * d
             ->   n * p <= Max / p.d * d.p, by multiplying by p on both sides.
             ->   n * p <= Max, because (Max / pd) * pd = pd q,
             where Max = pd q + r, with 0 <= r < pd
             So if n is positive, n <= Max / i * d, implies n * (i / d) <= Max.
             If n is negative, n >= - Max / i * d , implies -n <= Max / i * d
             which implies -n * p <= Max, see above.
             -n * p <= Max implies n * p >= -Max > Min.
          *)
          let max_n = Int.((max_value / i) * d) in
          if Int.(n > max_n || n < -max_n) then Bin_rep.Over_int(n, d)
          else f Int.O.(n * (i / d))

        let to_binable t =
          if t = V1.zero then Bin_rep.Zero
          else
            let num = t.num in
            let den = t.den in
            if not (Z.fits_int num && Z.fits_int den) then
              Bin_rep.Other t
            else
              (* Both num and den fits in an int each *)
              let n = Z.to_int num in (* Z.fits_int num *)
              let d = Z.to_int den in (* Z.fits_int den *)
              let ( = ) = Core_kernel.Std.Int.( = ) in
              let ( mod ) = Pervasives.( mod ) in
              if d = 0 then Bin_rep.Other t
              else if d = 1 then Bin_rep.Int n
              else if 10_000 mod d = 0
              then
                begin
                  if 100 mod d = 0
                  then if 10 mod d = 0
                    then check_overflow Bin_rep.over_10 ~n ~d 10
                    else check_overflow Bin_rep.over_100 ~n ~d 100
                  else if 1_000 mod d = 0
                  then check_overflow Bin_rep.over_1_000 ~n ~d 1_000
                  else check_overflow Bin_rep.over_10_000 ~n ~d 10_000
                end
              else if 100_000_000 mod d = 0 then
                begin
                  if 1_000_000 mod d = 0
                  then if 100_000 mod d = 0
                    then check_overflow Bin_rep.over_100_000 ~n ~d 100_000
                    else check_overflow Bin_rep.over_1_000_000 ~n ~d 1_000_000
                  else if 10_000_000 mod d = 0
                  then check_overflow Bin_rep.over_10_000_000 ~n ~d 10_000_000
                  else check_overflow Bin_rep.over_100_000_000 ~n ~d 100_000_000
                end
              else Bin_rep.Over_int (n, d)
        ;;

        let of_binable =
          let open Zarith.Q in
          function
          | Bin_rep.Zero    -> zero
          | Bin_rep.Int i   -> of_int i
          | Bin_rep.Over_int (n, d) -> of_ints n d
          | Bin_rep.Over_10 n -> of_ints n 10
          | Bin_rep.Over_100 n -> of_ints n 100
          | Bin_rep.Over_1_000 n -> of_ints n 1_000
          | Bin_rep.Over_10_000 n -> of_ints n 10_000
          | Bin_rep.Over_100_000 n -> of_ints n 100_000
          | Bin_rep.Over_1_000_000 n -> of_ints n 1_000_000
          | Bin_rep.Over_10_000_000 n -> of_ints n 10_000_000
          | Bin_rep.Over_100_000_000 n -> of_ints n 100_000_000
          | Bin_rep.Other o -> o
        ;;
      end)

    let bin_read_t buf ~pos_ref =
      match Tag.bin_read_t buf ~pos_ref with
      | Tag.Zero     -> zero
      | Tag.Int      -> of_int (Int.bin_read_t buf ~pos_ref)
      | Tag.Over_int ->
        let n = Int.bin_read_t buf ~pos_ref in
        let d = Int.bin_read_t buf ~pos_ref in
        of_ints n d
      | Tag.Over_10 ->
        let n = Int.bin_read_t buf ~pos_ref in
        of_ints n 10
      | Tag.Over_100 ->
        let n = Int.bin_read_t buf ~pos_ref in
        of_ints n 100
      | Tag.Over_1_000 ->
        let n = Int.bin_read_t buf ~pos_ref in
        of_ints n 1_000
      | Tag.Over_10_000 ->
        let n = Int.bin_read_t buf ~pos_ref in
        of_ints n 10_000
      | Tag.Over_100_000 ->
        let n = Int.bin_read_t buf ~pos_ref in
        of_ints n 100_000
      | Tag.Over_1_000_000 ->
        let n = Int.bin_read_t buf ~pos_ref in
        of_ints n 1_000_000
      | Tag.Over_10_000_000 ->
        let n = Int.bin_read_t buf ~pos_ref in
        of_ints n 10_000_000
      | Tag.Over_100_000_000 ->
        let n = Int.bin_read_t buf ~pos_ref in
        of_ints n 100_000_000
      | Tag.Other ->
        V1.bin_read_t buf ~pos_ref
    ;;

    let bin_reader_t = {
      bin_reader_t with Bin_prot.Type_class.read = bin_read_t ;
    }


  end
  (* Note V1 and V2 are the same type in ocaml.  The only thing
     that changes is the binprot representation.  This is safe (imho)
     as people declaring a stable type will have to explicitely referred
     to V1 or V2.  At a later point we can hide that V1 is equal to
     the regular type and thereby force people to switch to V2 or explicity
     call a of/to v1 function (which would be the identity) *)
  module Current = V2

  TEST_MODULE = struct
    open Core_kernel.Std

    let buf = Bigstring.create 256

    let roundtrip b =
      for pos = 0 to 17 do
        let (_ : int) = V1.bin_writer_t.Bin_prot.Type_class.write buf ~pos b in
        let result1  = V1.bin_reader_t.Bin_prot.Type_class.read buf ~pos_ref:(ref pos) in
        let (_ : int) = V2.bin_writer_t.Bin_prot.Type_class.write buf ~pos b in
        let result2  = V2.bin_reader_t.Bin_prot.Type_class.read buf ~pos_ref:(ref pos) in
        <:test_eq< V1.t >> b result1;
        <:test_eq< V2.t >> b result2;
      done
    ;;

    let test b =
      let open Core_kernel.Std in
      let v1 = Bin_prot.Writer.to_string V1.bin_writer_t b |> String.length in
      let v2 = Bin_prot.Writer.to_string V2.bin_writer_t b |> String.length in
      (* change to true if you want to see compaction rates during testing *)
      if false then
        printf "%s v1: %i v2: %i\n" (V1.sexp_of_t b |> Sexp.to_string_mach)
          v1 v2;
      roundtrip b;
    ;;

    (* This checks an axiom used in the proof of [check_overflow] *)
    TEST = Int.(-max_value > min_value)
    ;;

    (* This contains a test for all branches.*)
    TEST_UNIT = test Current.zero (* test for Zero *)
    TEST_UNIT = test Current.one (* test for Int *)
    TEST_UNIT = test Current.ten
    TEST_UNIT = test Current.hundred
    TEST_UNIT = test Current.thousand
    TEST_UNIT = test Current.million
    TEST_UNIT = test Current.billion
    TEST_UNIT = test Current.trillion

    let ( / ) = Current.( / )
    let ( * ) = Current.( * )
    (* Test for all Over_10^i *)
    TEST_UNIT = test (Current.one / Current.ten)
    TEST_UNIT = test (Current.one / Current.hundred)
    TEST_UNIT = test (Current.one / Current.thousand)
    TEST_UNIT = test (Current.one / (Current.thousand * Current.ten))
    TEST_UNIT = test (Current.one / (Current.thousand * Current.hundred))
    TEST_UNIT = test (Current.one / Current.million)
    TEST_UNIT = test (Current.one / (Current.million  * Current.ten))
    TEST_UNIT = test (Current.one / (Current.million  * Current.hundred))
    TEST_UNIT = test (Current.one / Current.billion)
    TEST_UNIT = test (Current.one / (Current.billion  * Current.ten))
    TEST_UNIT = test (Current.one / (Current.billion  * Current.hundred))
    TEST_UNIT = test (Current.one / Current.trillion)
    (* Test for Over_int *)
    TEST_UNIT = test ((Current.of_int 2) / (Current.of_int 3))
    (* Test for overflow  : 2^62 / 25 would be Over_100(2^64) and should overflow,
       and fallback on Other(2^62, 25) *)
    TEST_UNIT = test ((Current.mul_2exp Current.one 62) / (Current.of_int 25))
    TEST_UNIT = test ((Current.mul_2exp (Current.of_int (-1)) 62) / (Current.of_int 25))

    (* This test tests for overflow in the numerator *)
    TEST_UNIT = test ((Current.mul_2exp Current.one  65) / (Current.of_int 25))

    (* This test tests for overflow in the denominator *)
    TEST_UNIT = test (Current.one /  (Current.mul_2exp Current.one  65))

    (* Test for division by zero cases *)
    TEST_UNIT = test Current.nan
    TEST_UNIT = test Current.infinity
    TEST_UNIT = test Current.neg_infinity

    let numbers = [
      "-100.00000000";
      "100.00000000";
      "0.00000000";
      "-200.00000000";
      "200.00000000";
      "-300.00000000";
      "300.00000000";
      "-400.00000000";
      "-1000.00000000";
      "1000.00000000";
      "-1.00000000";
      "400.00000000";
      "-500.00000000";
      "1.00000000";
      "500.00000000";
      "-600.00000000";
      "-2000.00000000";
      "2.00000000";
      "-2.00000000";
      "600.00000000";
      "0.20720000";
      "-0.20227524";
      "0.18800000";
      "0.16550000";
      "0.15950000";
      "0.13000000";
      "0.12950000";
      "0.11950000";
      "-0.07232871";
      "0.05950000";
      "-0.05424653";
      "0.04600437";
      "0.04600000";
      "0.04050000";
      "-0.03616435";
      "0.03550391";
      "0.03550000";
      "0.02000000";
      "0.01950000";
      "0.01050000";
      "-316673.67291835";
      "217240000000.0";
      "-217240000000.0";
      "3423.123456789";
      "-3423.1234567891";
    ]
    ;;

    TEST_UNIT = List.iter numbers ~f:(fun s -> test (V1.of_string s))
    ;;
  end
end

module T = Stable.Current
include T
include Comparable.Make_binable (T)

let of_zarith_bigint = of_bigint
let to_zarith_bigint = to_bigint

let of_bigint big = of_zarith_bigint (Bigint.to_zarith_bigint big)

let num_as_bigint t = Bigint.of_zarith_bigint t.num
let den_as_bigint t = Bigint.of_zarith_bigint t.den

let to_int_exn = to_int

TEST = Int.equal (to_int_exn (of_int Int.max_value)) Int.max_value
TEST = Int.equal (to_int_exn (of_int Int.min_value)) Int.min_value
TEST =
  try ignore (to_int_exn (of_int Int.max_value + one)); false
  with Z.Overflow -> true
TEST =
  try ignore (to_int_exn (of_int Int.min_value - one)); false
  with Z.Overflow -> true

let to_int t = Option.try_with (fun () -> to_int_exn t)

(* according to Marcin this should cost us only 1 bit of precision *)
let to_float t = Float.(/) (Z.to_float t.num) (Z.to_float t.den)

let sum xs = List.fold xs ~init:zero ~f:(+)

let is_zero (x:t) = x = zero

let t_of_sexp = function
  | Sexp.Atom f -> of_string f
  | Sexp.List [Sexp.Atom f; Sexp.Atom "+"; Sexp.Atom prec_not_in_float] ->
    of_string f + of_rational_string prec_not_in_float
  | sexp ->
    Sexplib.Conv.of_sexp_error "expected a float" sexp
;;
TEST =
  let t = t_of_sexp (Sexp.of_string "(26.710790545 + 9999/100000000000000)") in
  let low_bound = of_string "26.710790545" in
  let high_bound = of_string "26.710790546" in
  t > low_bound && t < high_bound

let sign x = if x < zero then -1 else if x > zero then 1 else 0
let pp ppf t = Format.fprintf ppf "%s" (to_string t)

let inverse t = div one t
TEST = inverse one = one
TEST = inverse (neg one) = (neg one)
TEST = inverse ten = of_string ".1"

(* Exponentiation by repeated squaring, to calculate t^n in O(log n) multiplications. *)
let ( ** ) t pow =
  (* Invariant: [result * (squares ** n) = t ** pow].
     Termination: Reduces number of binary digits of [n] each iteration, so eventually
     [n = 0], at which point [result = result * (squares ** n) = t ** pow]. *)
  let rec loop result squares n =
    if Int.equal n 0
    then result
    else
    if Int.equal (n % 2) 0
    then loop result (squares * squares) (Int.(/) n 2)
    else loop (result * squares) (squares * squares) Int.((n - 1) / 2)
  in
  (* Int.abs Int.min_value < 0, so have to handle it separately.
     Although raising anything other than one to that power would probably eat your entire
     RAM pretty quickly.
  *)
  if Int.equal pow Int.min_value
  then inverse (loop t t Int.max_value)
  else if Int.(<) pow 0
  then inverse (loop one t (Int.abs pow))
  else loop one t pow
;;

TEST = ten ** 0 = one
TEST = ten ** 1 = ten
TEST = ten ** 2 = hundred
TEST = ten ** 3 = thousand
TEST = ten ** 6 = million
TEST = ten ** 9 = billion
TEST = ten ** 12 = trillion
TEST = ten ** (-2) = of_string "0.01"
TEST = one ** Int.min_value = one
TEST = of_string "2" ** 1000 = of_string
                                 ("107150860718626732094842504906000181056140481170553360744375038837035105112493612249"
                                  ^"319837881569585812759467291755314682518714528569231404359845775746985748039345677748"
                                  ^"242309854210746050623711418779541821530464749835819412673987675591655439460770629145"
                                  ^"71196477686542167660429831652624386837205668069376")

let half = of_ints 1 2

let truncate t = of_zarith_bigint (to_zarith_bigint t)

let floor t =
  let t' = truncate t in
  if t' > t then t' - one else t'
;;

(* This is quite a common case, and substantially faster than faffing around with
   [to_multiple_of] *)
let round_integer ?(dir=`Nearest) t =
  match dir with
  | `Zero -> truncate t
  | `Down -> floor t
  | `Up -> neg (floor (neg t))
  | `Nearest -> floor (t + half)
;;

let round ?dir ?to_multiple_of t =
  match to_multiple_of with
  | None -> round_integer ?dir t
  | Some to_multiple_of ->
    if is_zero to_multiple_of
    then failwith "Bignum.round: to_multiple_of may not be zero";
    to_multiple_of * round_integer ?dir (t / to_multiple_of)
;;

let iround ?dir ?to_multiple_of t =
  match to_multiple_of with
  | None -> to_int (round_integer ?dir t)
  | Some to_multiple_of ->
    if Int.equal 0 to_multiple_of
    then None
    else to_int (round ?dir ~to_multiple_of:(of_int to_multiple_of) t)
;;

let iround_exn ?dir ?to_multiple_of t =
  match to_multiple_of with
  | None -> to_int_exn (round_integer ?dir t)
  | Some to_multiple_of ->
    to_int_exn (round ?dir ~to_multiple_of:(of_int to_multiple_of) t)
;;

let round_as_bigint_exn ?dir ?to_multiple_of t =
  Bigint.of_zarith_bigint
    (match to_multiple_of with
     | None -> to_zarith_bigint (round_integer ?dir t)
     | Some to_multiple_of ->
       to_zarith_bigint (round ?dir ~to_multiple_of:(of_bigint to_multiple_of) t))
;;

let round_as_bigint ?dir ?to_multiple_of t =
  Option.try_with (fun () -> round_as_bigint_exn ?dir ?to_multiple_of t)
;;

let round_decimal ?dir ~digits t =
  if Int.equal 0 digits
  then round_integer ?dir t
  else round ?dir ~to_multiple_of:(tenth ** digits) t
;;

TEST_MODULE "round" = struct

  let x     = of_string "1.23456789"
  let neg_x = neg x
  ;;

  TEST = round                            x           = of_string "1"
  TEST = round ~to_multiple_of:tenth      x           = of_string "1.2"
  TEST = round ~to_multiple_of:hundredth  x           = of_string "1.23"
  TEST = round ~to_multiple_of:thousandth x           = of_string "1.235"
  TEST = round ~to_multiple_of:millionth  x           = of_string "1.234568"
  TEST = round                            neg_x       = of_string "-1"
  TEST = round ~to_multiple_of:tenth      neg_x       = of_string "-1.2"
  TEST = round ~to_multiple_of:hundredth  neg_x       = of_string "-1.23"
  TEST = round ~to_multiple_of:thousandth neg_x       = of_string "-1.235"
  TEST = round ~to_multiple_of:millionth  neg_x       = of_string "-1.234568"
  ;;

  TEST = round_decimal ~dir:`Nearest ~digits:0 x      = of_string "1"
  TEST = round_decimal ~dir:`Nearest ~digits:1 x      = of_string "1.2"
  TEST = round_decimal ~dir:`Nearest ~digits:2 x      = of_string "1.23"
  TEST = round_decimal ~dir:`Nearest ~digits:3 x      = of_string "1.235"
  TEST = round_decimal ~dir:`Nearest ~digits:4 x      = of_string "1.2346"
  TEST = round_decimal ~dir:`Nearest ~digits:0 neg_x  = of_string "-1"
  TEST = round_decimal ~dir:`Nearest ~digits:1 neg_x  = of_string "-1.2"
  TEST = round_decimal ~dir:`Nearest ~digits:2 neg_x  = of_string "-1.23"
  TEST = round_decimal ~dir:`Nearest ~digits:3 neg_x  = of_string "-1.235"
  TEST = round_decimal ~dir:`Nearest ~digits:4 neg_x  = of_string "-1.2346"
  ;;

  TEST = round_decimal ~dir:`Up ~digits:0 x      = of_string "2"
  TEST = round_decimal ~dir:`Up ~digits:1 x      = of_string "1.3"
  TEST = round_decimal ~dir:`Up ~digits:2 x      = of_string "1.24"
  TEST = round_decimal ~dir:`Up ~digits:3 x      = of_string "1.235"
  TEST = round_decimal ~dir:`Up ~digits:4 x      = of_string "1.2346"
  TEST = round_decimal ~dir:`Up ~digits:0 neg_x  = of_string "-1"
  TEST = round_decimal ~dir:`Up ~digits:1 neg_x  = of_string "-1.2"
  TEST = round_decimal ~dir:`Up ~digits:2 neg_x  = of_string "-1.23"
  TEST = round_decimal ~dir:`Up ~digits:3 neg_x  = of_string "-1.234"
  TEST = round_decimal ~dir:`Up ~digits:4 neg_x  = of_string "-1.2345"
  ;;

  TEST = round_decimal ~dir:`Down ~digits:0 x      = of_string "1"
  TEST = round_decimal ~dir:`Down ~digits:1 x      = of_string "1.2"
  TEST = round_decimal ~dir:`Down ~digits:2 x      = of_string "1.23"
  TEST = round_decimal ~dir:`Down ~digits:3 x      = of_string "1.234"
  TEST = round_decimal ~dir:`Down ~digits:4 x      = of_string "1.2345"
  TEST = round_decimal ~dir:`Down ~digits:0 neg_x  = of_string "-2"
  TEST = round_decimal ~dir:`Down ~digits:1 neg_x  = of_string "-1.3"
  TEST = round_decimal ~dir:`Down ~digits:2 neg_x  = of_string "-1.24"
  TEST = round_decimal ~dir:`Down ~digits:3 neg_x  = of_string "-1.235"
  TEST = round_decimal ~dir:`Down ~digits:4 neg_x  = of_string "-1.2346"
  ;;

  TEST = round_decimal ~dir:`Zero ~digits:0 x      = of_string "1"
  TEST = round_decimal ~dir:`Zero ~digits:1 x      = of_string "1.2"
  TEST = round_decimal ~dir:`Zero ~digits:2 x      = of_string "1.23"
  TEST = round_decimal ~dir:`Zero ~digits:3 x      = of_string "1.234"
  TEST = round_decimal ~dir:`Zero ~digits:4 x      = of_string "1.2345"
  TEST = round_decimal ~dir:`Zero ~digits:0 neg_x  = of_string "-1"
  TEST = round_decimal ~dir:`Zero ~digits:1 neg_x  = of_string "-1.2"
  TEST = round_decimal ~dir:`Zero ~digits:2 neg_x  = of_string "-1.23"
  TEST = round_decimal ~dir:`Zero ~digits:3 neg_x  = of_string "-1.234"
  TEST = round_decimal ~dir:`Zero ~digits:4 neg_x  = of_string "-1.2345"
  ;;

  TEST = try ignore (round ~to_multiple_of:zero one : t); false with _ -> true
  TEST = Option.is_none (iround ~to_multiple_of:0 one)
  TEST = try ignore (iround_exn ~to_multiple_of:0 one : int); false with _ -> true

  let dir_to_string = function
    | `Up -> "up"
    | `Down -> "down"
    | `Nearest -> "nearest"
    | `Zero -> "zero"

  let as_float f =
    List.iter [`Up; `Down; `Nearest; `Zero] ~f:(fun dir ->
      <:test_result< float >> ~message:(dir_to_string dir)
        ~expect:(Float.round ~dir f)
        (to_float (round ~dir (of_float f))))

  TEST_UNIT =
    List.iter [0.; 0.5; 99.5; 99.99; 1_000.]
      ~f:(fun f -> as_float f; as_float (Float.neg f))

  TEST_MODULE "iround" = struct
    let as_int ~to_multiple_of i =
      List.iter [`Up; `Down; `Nearest; `Zero] ~f:(fun dir ->
        <:test_result< int >> ~message:(dir_to_string dir)
          ~expect:(Int.round ~dir ~to_multiple_of i)
          (iround_exn ~dir ~to_multiple_of (of_int i)))

    TEST_UNIT =
      List.iter [1; 327; 1_000_012] ~f:(fun to_multiple_of ->
        List.iter [0; 1; 3315; 98_765_432] ~f:(fun i ->
          as_int ~to_multiple_of i;
          as_int ~to_multiple_of (Int.neg i)))

    TEST_UNIT = as_int ~to_multiple_of:1 Int.max_value
    TEST_UNIT = as_int ~to_multiple_of:1 Int.min_value

    let overflows t =
      <:test_pred< int option >> Option.is_none (iround t);
      try ignore (iround_exn t : int); false with _ -> true

    TEST = overflows (of_int Int.max_value + one)
    TEST = overflows (of_int Int.min_value - one)
  end
end

include (Hashable.Make_binable (struct
           include T

           let compare = compare
           let hash    = Hashtbl.hash
         end) : Hashable.S_binable with type t := t)

module O = struct
  let ( + ) = ( + )
  let ( - ) = ( - )
  let ( / ) = ( / )
  let ( * ) = ( * )
  let ( ** ) = ( ** )

  include (Replace_polymorphic_compare :
             Core_kernel.Polymorphic_compare_intf.Infix with type t := t)

  let abs = abs
  let neg = neg

  let zero      = zero
  let one       = one
  let ten       = ten
  let hundred   = hundred
  let thousand  = thousand
  let million   = million
  let billion   = billion
  let trillion  = trillion

  let tenth      = tenth
  let hundredth  = hundredth
  let thousandth = thousandth
  let millionth  = millionth
  let billionth  = billionth
  let trillionth = trillionth

  let of_int    = of_int
  let of_float  = of_float
end



let rec gen_stern_brocot ~lower_numer ~lower_denom ~upper_numer ~upper_denom =
  let open Quickcheck.Generator in
  let numer = Bigint.(lower_numer + upper_numer) in
  let denom = Bigint.(lower_denom + upper_denom) in
  union
    [ of_fun (fun () ->
        gen_stern_brocot
          ~lower_numer
          ~lower_denom
          ~upper_numer:numer
          ~upper_denom:denom)
    ; singleton (of_bigint numer / of_bigint denom)
    ; of_fun (fun () ->
        gen_stern_brocot
          ~lower_numer:numer
          ~lower_denom:denom
          ~upper_numer
          ~upper_denom)
    ]

let gen_greater_than x =
  let open Quickcheck.Generator in
  gen_stern_brocot
    ~lower_numer:Bigint.one
    ~lower_denom:Bigint.one
    ~upper_numer:Bigint.one
    ~upper_denom:Bigint.zero
  >>| fun y ->
  (x * y)

let gen_less_than x =
  let open Quickcheck.Generator in
  gen_greater_than (neg x) >>| neg

let gen_between_exclusive x y =
  let open Quickcheck.Generator in
  gen_stern_brocot
    ~lower_numer:Bigint.zero
    ~lower_denom:Bigint.one
    ~upper_numer:Bigint.one
    ~upper_denom:Bigint.one
  >>| fun z ->
  (x + (z * (y - x)))

let gen_with_negative_and_inverse positive =
  let open Quickcheck.Generator in
  let negative = neg positive in
  of_list [ positive ; negative ; one / positive ; one / negative ]

let rec gen_greater_than_candidates prev bignums =
  let open Quickcheck.Generator in
  match bignums with
  | [] -> gen_greater_than prev
  | next :: rest ->
    union
      [ gen_between_exclusive prev next
      ; singleton next
      ; of_fun (fun () -> gen_greater_than_candidates next rest)
      ]

let gen_finite =
  let open Quickcheck.Generator in
  union
    [ of_list [ zero ; one ; neg one ]
    ; gen_greater_than_candidates one
        [ ten ; hundred ; thousand ; million ; billion ; trillion ]
      >>= gen_with_negative_and_inverse
    ]

let gen =
  let open Quickcheck.Generator in
  union
    [ of_list [ one / zero ; neg one / zero ; zero / zero ]
    ; gen_finite
    ]

let gen_between ~with_undefined ~lower_bound ~upper_bound =
  let open Quickcheck.Generator in
  let undef =
    if with_undefined
    then [ 1., singleton (zero / zero) ]
    else []
  in
  let lower_inclusive =
    match lower_bound with
    | Unbounded -> [ 1., singleton (neg one / zero) ]
    | Incl lower -> [1., singleton lower]
    | Excl _ -> []
  in
  let upper_inclusive =
    match upper_bound with
    | Unbounded -> [ 1., singleton (one / zero) ]
    | Incl upper -> [1., singleton upper]
    | Excl _ -> []
  in
  let between_exclusive =
    match lower_bound, upper_bound with
    | Unbounded, Unbounded -> gen_finite
    | Unbounded, (Incl upper | Excl upper) ->
      gen_less_than upper
    | (Incl lower | Excl lower), Unbounded ->
      gen_greater_than lower
    | (Incl lower | Excl lower), (Incl upper | Excl upper) ->
      gen_between_exclusive lower upper
  in
  weighted_union ([ 10., between_exclusive ] @ lower_inclusive @ upper_inclusive @ undef)

let rec obs_stern_brocot ~lower_numer ~lower_denom ~upper_numer ~upper_denom =
  let open Quickcheck.Observer in
  let numer = Bigint.(lower_numer + upper_numer) in
  let denom = Bigint.(lower_denom + upper_denom) in
  let bignum = (of_bigint numer / of_bigint denom) in
  comparison ~compare:compare
    ~eq:bignum
    ~lt:(of_fun (fun () -> obs_stern_brocot
                             ~lower_numer ~lower_denom
                             ~upper_numer:numer ~upper_denom:denom))
    ~gt:(of_fun (fun () -> obs_stern_brocot
                             ~lower_numer:numer ~lower_denom:denom
                             ~upper_numer ~upper_denom))
    ~compare_sexp:(fun () -> Sexp.Atom "Bignum.compare")
    ~sexp_of_eq:sexp_of_t

let obs_between_exclusive x y =
  let open Quickcheck.Observer in
  unmap
    (obs_stern_brocot
       ~lower_numer:Bigint.zero
       ~lower_denom:Bigint.one
       ~upper_numer:Bigint.one
       ~upper_denom:Bigint.one)
    ~f:(fun z -> ((z - x) / (y - x)))
    ~f_sexp:(fun () ->
      <:sexp_of< [`map_unit_range_to_between of t * t] >>
        (`map_unit_range_to_between (x, y)))

let obs_greater_than x =
  let open Quickcheck.Observer in
  unmap
    (obs_stern_brocot
       ~lower_numer:Bigint.one
       ~lower_denom:Bigint.one
       ~upper_numer:Bigint.one
       ~upper_denom:Bigint.zero)
    ~f:(fun z -> (z / x))
    ~f_sexp:(fun () -> <:sexp_of< [`divided_by of t] >> (`divided_by x))

let rec obs_greater_than_candidates prev bignums =
  let open Quickcheck.Observer in
  match bignums with
  | [] -> obs_greater_than prev
  | next :: rest ->
    comparison ~compare
      ~eq:next
      ~lt:(obs_between_exclusive prev next)
      ~gt:(obs_greater_than_candidates next rest)
      ~compare_sexp:(fun () -> Sexp.Atom "Bignum.compare")
      ~sexp_of_eq:sexp_of_t

(* [obs_positive_greater_than_one] produces observers that distinguish larger numbers
   in fewer steps than [obs_greater_than one]. The latter only distinguishes 1,000
   from 1,001 at a recursion depth of 1,000.  With the candidates approach, it does so at
   a depth of about 3. *)
let obs_positive_greater_than_one =
  obs_greater_than_candidates one
    [ ten ; hundred ; thousand ; million ; billion ; trillion ]

let obs_positive_less_than_one =
  let open Quickcheck.Observer in
  unmap obs_positive_greater_than_one
    ~f:inverse
    ~f_sexp:(fun () -> Sexp.Atom "Bignum.inverse")

let obs_positive =
  let open Quickcheck.Observer in
  comparison ~compare
    ~eq:one
    ~lt:obs_positive_less_than_one
    ~gt:obs_positive_greater_than_one
    ~compare_sexp:(fun () -> Sexp.Atom "Bignum.compare")
    ~sexp_of_eq:sexp_of_t

let obs_negative =
  let open Quickcheck.Observer in
  unmap obs_positive ~f:neg ~f_sexp:(fun () -> Sexp.Atom "Bignum.neg")

let obs_finite =
  let open Quickcheck.Observer in
  comparison ~compare
    ~eq:zero
    ~lt:obs_negative
    ~gt:obs_positive
    ~compare_sexp:(fun () -> Sexp.Atom "Bignum.compare")
    ~sexp_of_eq:sexp_of_t

let is_finite bignum =
  not (is_zero (den bignum))

let obs =
  let open Quickcheck.Observer in
  of_predicate ~f:is_finite ~f_sexp:(fun () -> Sexp.Atom "bignum_is_finite")
    obs_finite
    (of_list [ zero / zero ; one / zero ; neg one / zero ] ~equal
       ~sexp_of_elt:sexp_of_t)
