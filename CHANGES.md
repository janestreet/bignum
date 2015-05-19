## 112.35.00

- Upgraded from Zarith 1.2 to 1.3.
- Removed dependence on `Big_int`.

## 112.24.00

- Fixed exception raised by `Bignum.sexp_of_t` when the denominator is zero.

## 112.17.00

- Added `Bigint.random` function, which produces a uniformly
  distributed value.

## 112.06.00

- Added functions to round from `Bignum.t` to `Bigint.t`, and to convert
  `Bigint.t` into `Bignum.t`.

## 112.01.00

- Added `Bignum.Bigint` module, with arbitrary-precision integers
  based on `Zarith`, which is significantly faster than the
  `Num.Big_int` library.

## 111.17.00

- Improved the performance of binprot deserialization by removing the
  allocation of an intermediate type.

## 111.13.00

- Eliminated the dependence of `Bignum` on `Re2`, and reduced dependence
  from `Core` to `Core_kernel`.
- Extended the rounding interface to bring it in line with int and float
  rounding.
- Improved the performance of `Bignum`'s binprot.

    `Bignum`'s binprot had been to just binprot the decimal string
    representation.  This is both slow to do and unnecessarily big in
    the majority of cases.  Did something better in the majority of
    cases and fell back to this representation in the exceptional case.

        $ ./inline_benchmarks_runner
        Estimated testing time 20s (2 benchmarks x 10s). Change using -quota SECS.

    | Name                                                 | Time/Run |   mWd/Run | Percentage |
    |------------------------------------------------------|----------|-----------|------------|
    | bignum0.ml:Stable:Bignum binprot roundtrip compact   |   7.87us |   490.00w |     32.88% |
    | bignum0.ml:Stable:Bignum binprot roundtrip classic   |  23.94us | 1_079.00w |    100.00% |

