#use "topfind";;
#require "js-build-tools.oasis2opam_install";;

open Oasis2opam_install;;

generate ~package:"bignum"
  [ oasis_lib "bignum"
  ; file "META" ~section:"lib"
  ; file "_build/namespace_wrappers/zarith_1_4.cmi" ~section:"lib"
  ]
