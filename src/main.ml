#open "globals";;
#open "compiler";;

let set_zlc s =
  core_lib := s
and set_output s =
  output_lib := s
and add_include d =
  load_path := d :: !load_path
and open_set set =
  try
    modules__default_used_modules := assoc set default_used_interfaces
  with Not_found ->
    failwith ("unknown module set " ^ set)
and log_mode () =
  verbose := true
and no_warnings() =
  warn_flag := false
;;

let main() =
try
  sys__catch_break true;
  modules__default_used_modules := assoc "cautious" default_used_interfaces;
  load_path := [];
  arg__parse ["-clib", arg__String set_zlc;
              "-o", arg__String set_output;
              "-c", arg__String merge_library;
              "-I", arg__String add_include;
              "-include", arg__String add_include;
              "-O", arg__String open_set;
              "-open", arg__String open_set;
              "-L", arg__Unit log_mode;
              "-W", arg__Unit no_warnings;
              "-", arg__String anonymous]
             anonymous;
  exit 0
with Failure s -> prerr_endline ("Fatal error: " ^ s ^ "."); exit 2
   | sys__Break -> exit 2
   | sys__Sys_error msg ->
      prerr_endline ("Input/output error: " ^ msg ^ ".");
      exit 2
   | e -> prerr_endline "Unexpected exception. Please report."; raise e
;;

printexc__f main (); exit 0;;
