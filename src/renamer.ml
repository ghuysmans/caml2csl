
#open "filename";;

let rename s=
  if check_suffix s ".ml" then
      let name=chop_suffix s ".ml" in sys__rename s (name ^ ".caml")
  else if check_suffix s ".mli" then
      let name=chop_suffix s ".mli" in sys__rename s (name ^ ".camli")
  else prerr_endline ("Don't know what to do with " ^ s)
;;

arg__parse [] rename;;
