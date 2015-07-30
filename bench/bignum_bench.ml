open Core.Std
open Bignum.Std
open Bignum

let numbers =
  List.map ~f:of_string
    [ "-100.00000000";
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
      "3423.123456789";
      "-3423.1234567891";
    ]
;;

BENCH_MODULE "Bignum binprot" = struct

  let buf = Bigstring.create 128

  BENCH "roundtrip compact" = List.iter numbers ~f:(fun b ->
    let (_ : int) =
      Stable.V2.bin_writer_t.Bin_prot.Type_class.write buf ~pos:0 b
    in
    let (_ : Stable.V2.t) =
      Stable.V2.bin_reader_t.Bin_prot.Type_class.read buf ~pos_ref:(ref 0)
    in
    ())
  ;;

  BENCH "roundtrip classic" = List.iter numbers ~f:(fun b ->
    let (_ : int) =
      Stable.V1.bin_writer_t.Bin_prot.Type_class.write buf ~pos:0 b
    in
    let (_ : Stable.V1.t) =
      Stable.V1.bin_reader_t.Bin_prot.Type_class.read buf ~pos_ref:(ref 0)
    in
    ())
  ;;
end

BENCH_MODULE "round" = struct
  BENCH_INDEXED "round_decimal" digits [0;3;6;9] =
    fun () ->
    List.iter numbers ~f:(fun number -> ignore (round_decimal number ~digits : t))
  ;;

  BENCH "round" =
    List.iter numbers ~f:(fun number -> ignore (round number : t))
  ;;
end


