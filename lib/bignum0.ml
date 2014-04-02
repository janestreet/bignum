open Core.Std

module Stable = struct
  module V1 = struct
    module Zarith = Zarith_1_2
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

    let trillion =
      if Int.(=) Sys.word_size 64
      then of_int 1_000_000_000_000
      else of_float 1_000_000_000_000.
    ;;

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

    let to_string = to_float_string

    let of_float_string =
      let int_rex     = Re2.Regex.create_exn "^(\\+|-)?[0-9]+$" in
      let valid_parts = Re2.Regex.create_exn "^(\\+|-)?[0-9]*\\.[0-9]*$" in
      (fun s ->
        if Re2.Regex.matches int_rex s
        then of_rational_string s
        else begin
          try
            if not (Re2.Regex.matches valid_parts s)
            then failwithf "unable to parse %s as Bignum.t" s ();
            begin match String.lsplit2 ~on:'.' s with
            | Some (int_part, frac_part) ->
                if String.(=) frac_part ""
                then of_rational_string int_part
                else begin
                  let frac_len  = String.length frac_part in
                  let den       = Z.pow (Z.of_int 10) frac_len in
                  let int_part  = Z.mul (Z.of_string int_part) den in
                  let frac_part = Z.of_string frac_part in
                  let num =
                    if Char.(=) s.[0] '-'
                    then Z.sub int_part frac_part
                    else Z.add int_part frac_part
                  in
                  make num den
                end
            | None -> assert false
            end
          with
          | e -> failwithf "unable to convert %s to Bignum.t : %s" s (Exn.to_string e) ()
        end)
    ;;

    let of_string s =
      try
        let s =
          if String.contains s '_'
          then String.concat ~sep:"" (String.split ~on:'_' s)
          else s
        in
        let s = String.lowercase s in (* 'E' -> 'e' *)
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

    TEST = to_string (of_string "0")          = "0"
    TEST = to_string (of_string "0.01")       = "0.01"
    TEST = to_string (of_string ".01")        = "0.01"
    TEST = to_string (of_string "-.01")       = "-0.01"
    TEST = to_string (of_string "-0.01")      = "-0.01"
    TEST = to_string (of_string "1")          = "1"
    TEST = to_string (of_string "1.2")        = "1.2"
    TEST = to_string (of_string "1.23")       = "1.23"
    TEST = to_string (of_string "1.234")      = "1.234"
    TEST = to_string (of_string "1.2345")     = "1.2345"
    TEST = to_string (of_string "1.23456")    = "1.23456"
    TEST = to_string (of_string "1.234567")   = "1.234567"
    TEST = to_string (of_string "1.2345678")  = "1.2345678"
    TEST = to_string (of_string "1.23456789") = "1.23456789"
    TEST = to_string (of_string "-1")          = "-1"
    TEST = to_string (of_string "-1.2")        = "-1.2"
    TEST = to_string (of_string "-1.23")       = "-1.23"
    TEST = to_string (of_string "-1.234")      = "-1.234"
    TEST = to_string (of_string "-1.2345")     = "-1.2345"
    TEST = to_string (of_string "-1.23456")    = "-1.23456"
    TEST = to_string (of_string "-1.234567")   = "-1.234567"
    TEST = to_string (of_string "-1.2345678")  = "-1.2345678"
    TEST = to_string (of_string "-1.23456789") = "-1.23456789"

    TEST = of_string "1.2e5" = of_int 120_000
    TEST = of_string "1.2E5" = of_int 120_000
    TEST = of_string "-0.2e0" = of_string "-0.2"
    TEST = of_string "10e-3" = one / hundred

    TEST = to_string (of_int 7 / of_int 9)     = "0.777777777"
    TEST = to_string (of_int (-7) / of_int 9)  = "-0.777777777"
    TEST = (of_float 766.46249999999997726) <> (of_float 766.462499999999864)


    let sexp_of_t t =
      try
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

    let t_of_sexp s =
      match s with
      | Sexp.Atom s -> of_string s
      | Sexp.List [Sexp.Atom float_part; Sexp.Atom "+"; Sexp.Atom rational_part] ->
        let t1 = of_float_string float_part in
        let t2 = of_rational_string rational_part in
        add t1 t2
      | Sexp.List _ -> of_sexp_error "expected Atom or List [float; \"+\"; remainder]" s
    ;;

    include Bin_prot.Utils.Make_binable (struct
      type t' = t
      type t  = t'
      module Binable = struct
        type t = string with bin_io
      end

      let to_binable t = Zarith.Q.to_string t
      let of_binable s = Zarith.Q.of_string s
    end)
  end
  module Current = V1
end


module T = Stable.Current
include T
include Comparable.Make_binable (T)

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
let __UNUSED_VALUE__is_inf _t = false

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

let __UNUSED_VALUE__of_rational x = x
let __UNUSED_VALUE__to_rational x = x

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

let truncate t places =
  let decimal_mover = ten ** places in
  (of_bigint (to_bigint (decimal_mover * t)) / decimal_mover)
;;
TEST = truncate (of_string "1.23456789") 0 = of_string "1"
TEST = truncate (of_string "1.23456789") 1 = of_string "1.2"
TEST = truncate (of_string "1.23456789") 2 = of_string "1.23"
TEST = truncate (of_string "1.23456789") 3 = of_string "1.234"
TEST = truncate (of_string "1.23456789") 4 = of_string "1.2345"
TEST = truncate (of_string "-1.23456789") 0 = of_string "-1"
TEST = truncate (of_string "-1.23456789") 1 = of_string "-1.2"
TEST = truncate (of_string "-1.23456789") 2 = of_string "-1.23"
TEST = truncate (of_string "-1.23456789") 3 = of_string "-1.234"
TEST = truncate (of_string "-1.23456789") 4 = of_string "-1.2345"

let floor t =
  let t' = truncate t 0 in
  if t' > t then t' - one else t'
TEST = floor (of_string "123456.789") = (of_string "123456")
TEST = floor (of_string "-123456.789") = of_string "-123457"

let round =
  let one_half = of_float 0.5 in
  (fun t places ->
    let decimal_mover = ten ** places in
    floor (t * decimal_mover + one_half) / decimal_mover)
;;
TEST = round (of_string "1.23456789") 0  = of_string "1"
TEST = round (of_string "1.23456789") 1  = of_string "1.2"
TEST = round (of_string "1.23456789") 2  = of_string "1.23"
TEST = round (of_string "1.23456789") 3  = of_string "1.235"
TEST = round (of_string "1.23456789") 4  = of_string "1.2346"
TEST = round (of_string "-1.23456789") 0 = of_string "-1"
TEST = round (of_string "-1.23456789") 1 = of_string "-1.2"
TEST = round (of_string "-1.23456789") 2 = of_string "-1.23"
TEST = round (of_string "-1.23456789") 3 = of_string "-1.235"
TEST = round (of_string "-1.23456789") 4 = of_string "-1.2346"

TEST = round (of_string "0.5") 0 = of_float (Float.round 0.5)
TEST = round (of_string "-0.5") 0 = of_float (Float.round (-0.5))

include (Hashable.Make_binable (struct
  include T

  let compare = compare
  let __UNUSED_VALUE__equal   = equal
  let hash    = Hashtbl.hash
end) : Hashable.S_binable with type t := t)

module O = struct
  let ( + ) = ( + )
  let ( - ) = ( - )
  let ( / ) = ( / )
  let ( * ) = ( * )
  let ( ** ) = ( ** )

  include (Replace_polymorphic_compare : Core_kernel.Polymorphic_compare_intf.Infix with type t := t)

  let abs = abs
  let neg = neg
  let round = round

  let zero      = zero
  let one       = one
  let ten       = ten
  let hundred   = hundred
  let thousand  = thousand
  let million   = million
  let billion   = billion
  let trillion  = trillion

  let of_int    = of_int
  let of_float  = of_float
end
