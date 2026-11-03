open! Core
open Expect_test_helpers_core

module type S = sig
  [%%rederive: type t = Bignum.t [@@deriving equal]]
end

module type S_container = sig
  type t [@@deriving equal]

  val singleton : Bignum.t -> t
end

module Via_container (M : S_container) = struct
  let equal a b =
    M.equal (M.singleton (Bignum.globalize a)) (M.singleton (Bignum.globalize b))
  ;;
end

let nan = Bignum.create ~num:Bigint.zero ~den:Bigint.zero
let%expect_test _ = require (Bignum.is_nan nan)

let test ?cr (module M : S) =
  let nan_equals_nan = M.equal nan nan in
  match Bool.equal nan_equals_nan nan_equals_nan with
  | false ->
    print_cr ?cr [%message "inconsistent" (nan_equals_nan : bool) (nan_equals_nan : bool)]
  | true ->
    (match nan_equals_nan with
     | false -> print_cr ?cr [%message "irreflexive" (nan_equals_nan : bool)]
     | true -> print_s [%message "" (nan_equals_nan : bool)])
;;

let%expect_test _ =
  test (module Bignum);
  [%expect {| (nan_equals_nan true) |}];
  test (module Bignum.Replace_polymorphic_compare);
  [%expect {| (nan_equals_nan true) |}];
  test (module Bignum.Unstable);
  [%expect {| (nan_equals_nan true) |}];
  test (module Bignum.Stable.V1);
  [%expect {| (nan_equals_nan true) |}];
  test (module Bignum.Stable.V2);
  [%expect {| (nan_equals_nan true) |}];
  test (module Bignum.Stable.V3);
  [%expect {| (nan_equals_nan true) |}];
  test (module Via_container (Bignum.Set));
  [%expect {| (nan_equals_nan true) |}];
  test
    (module Via_container (struct
        include Bignum.Hash_set

        let singleton n = of_list [ n ]
      end));
  [%expect {| (nan_equals_nan true) |}];
  test
    (module Via_container (struct
        type t = unit Bignum.Map.t [@@deriving equal]

        let singleton n = Bignum.Map.of_alist_exn [ n, () ]
      end));
  [%expect {| (nan_equals_nan true) |}];
  test
    (module Via_container (struct
        type t = unit Bignum.Table.t [@@deriving equal]

        let singleton n = Bignum.Table.of_alist_exn [ n, () ]
      end));
  [%expect {| (nan_equals_nan true) |}];
  ()
;;
