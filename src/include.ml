
load_object "translator";;

#open "globals";;
#open "lexer";;
#open "parser";;
#open "location";;
#open "syntax";;
#open "print";;
#open "modules";;
#open "emit";;

#open "conv";;
#open "changes";;
#open "enter";;
#open "compiler";;
#open "parse_mlc";;
#open "genlex";;

#open "hashtbl";;
#open "lexing";;




let simpl_name x sfx=
  if filename__check_suffix x sfx
   then filename__chop_suffix x sfx
   else x
;;

let set_zlc s =
  core_lib := s
and add_include d =
  load_path := d :: !load_path
and open_set set =
  try
    default_used_modules := assoc set default_used_interfaces
  with Not_found ->
    raise (arg__Bad ("unknown module set " ^ set))
and log_mode () =
  verbose := true
;;


default_used_modules := assoc "cautious" default_used_interfaces;
load_path := [];

add_include "/home/pauillac/coq2/barras/lib/caml2csl";;
set_zlc "/home/pauillac/coq2/barras/lib/caml2csl/std.zlc";;

reset_infix();;
reset_modules();;
