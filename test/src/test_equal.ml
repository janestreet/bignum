open! Core
open Expect_test_helpers_core

module type S = sig
  [%%rederive: type t = Bignum.t [@@deriving equal ~localize]]
end

module type S_container = sig
  type%template t [@@deriving equal [@mode local]]

  val singleton : Bignum.t -> t
end

module Via_container (M : S_container) = struct
  let%template[@mode l = (local, global)] equal a b =
    (M.equal [@mode l])
      (M.singleton (Bignum.globalize a))
      (M.singleton (Bignum.globalize b))
  ;;
end

let nan = Bignum.create ~num:Bigint.zero ~den:Bigint.zero
let%expect_test _ = require (Bignum.is_nan nan)

let test ?cr (module M : S) =
  let%template[@mode l = (local, global)] nan_equals_nan = (M.equal [@mode l]) nan nan in
  match Bool.equal nan_equals_nan (nan_equals_nan [@mode local]) with
  | false ->
    print_cr
      ?cr
      [%message
        "inconsistent" (nan_equals_nan : bool) (nan_equals_nan [@mode local] : bool)]
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
        type t = unit Bignum.Map.t [@@deriving equal ~localize]

        let singleton n = Bignum.Map.of_alist_exn [ n, () ]
      end));
  [%expect {| (nan_equals_nan true) |}];
  test
    (module Via_container (struct
        type t = unit Bignum.Table.t [@@deriving equal ~localize]

        let singleton n = Bignum.Table.of_alist_exn [ n, () ]
      end));
  [%expect {| (nan_equals_nan true) |}];
  ()
;;
