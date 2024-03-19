open! Core

module type For_quickcheck = sig
  type t [@@deriving quickcheck]
end

module type For_sexp = sig
  type t [@@deriving sexp, sexp_grammar]
end

let runtest
  (type t)
  (module Q : For_quickcheck with type t = t)
  (module S : For_sexp with type t = t)
  =
  let module M = struct
    include Q
    include S
  end
  in
  Sexp_grammar_validation.validate_grammar (module M)
  |> [%sexp_of: unit Or_error.t]
  |> print_s
;;

let%expect_test _ =
  runtest (module Bignum) (module Bignum);
  [%expect
    {|
    (Union
     (Float
      (Variant
       ((case_sensitivity Case_insensitive)
        (clauses ((No_tag ((name +) (clause_kind Atom_clause)))))))
      (List
       (Cons
        (Union
         (Float
          (Variant
           ((case_sensitivity Case_insensitive)
            (clauses ((No_tag ((name +) (clause_kind Atom_clause)))))))))
        (Cons
         (Variant
          ((case_sensitivity Case_insensitive)
           (clauses ((No_tag ((name +) (clause_kind Atom_clause)))))))
         (Cons String Empty))))))
    (Ok ())
    |}]
;;

let%expect_test _ =
  runtest (module Bignum) (module Bignum.Stable.V1);
  [%expect
    {|
    (Union
     (Float
      (Variant
       ((case_sensitivity Case_insensitive)
        (clauses ((No_tag ((name +) (clause_kind Atom_clause)))))))
      (List
       (Cons
        (Union
         (Float
          (Variant
           ((case_sensitivity Case_insensitive)
            (clauses ((No_tag ((name +) (clause_kind Atom_clause)))))))))
        (Cons
         (Variant
          ((case_sensitivity Case_insensitive)
           (clauses ((No_tag ((name +) (clause_kind Atom_clause)))))))
         (Cons String Empty))))))
    (Ok ())
    |}]
;;

let%expect_test _ =
  runtest (module Bignum) (module Bignum.Stable.V2);
  [%expect
    {|
    (Union
     (Float
      (Variant
       ((case_sensitivity Case_insensitive)
        (clauses ((No_tag ((name +) (clause_kind Atom_clause)))))))
      (List
       (Cons
        (Union
         (Float
          (Variant
           ((case_sensitivity Case_insensitive)
            (clauses ((No_tag ((name +) (clause_kind Atom_clause)))))))))
        (Cons
         (Variant
          ((case_sensitivity Case_insensitive)
           (clauses ((No_tag ((name +) (clause_kind Atom_clause)))))))
         (Cons String Empty))))))
    (Ok ())
    |}]
;;

let%expect_test _ =
  runtest (module Bignum) (module Bignum.Stable.V3);
  [%expect
    {|
    (Union
     (Float
      (Variant
       ((case_sensitivity Case_insensitive)
        (clauses ((No_tag ((name +) (clause_kind Atom_clause)))))))
      (List
       (Cons
        (Union
         (Float
          (Variant
           ((case_sensitivity Case_insensitive)
            (clauses ((No_tag ((name +) (clause_kind Atom_clause)))))))))
        (Cons
         (Variant
          ((case_sensitivity Case_insensitive)
           (clauses ((No_tag ((name +) (clause_kind Atom_clause)))))))
         (Cons String Empty))))))
    (Ok ())
    |}]
;;
