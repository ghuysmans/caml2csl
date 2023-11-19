
open Filename;;

let rename s=
  if check_suffix s ".ml" then
      let name=chop_suffix s ".ml" in Sys.rename s (name ^ ".caml")
  else if check_suffix s ".mli" then
      let name=chop_suffix s ".mli" in Sys.rename s (name ^ ".camli")
  else prerr_endline ("Don't know what to do with " ^ s)
;;

Caml__csl.arg_parse [] rename;;
