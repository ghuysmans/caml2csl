open Globals;;
open Compiler;;

let set_zlc s =
  core_lib := s
and set_output s =
  output_lib := s
and add_include d =
  load_path := d :: !load_path
and open_set set =
  try
    Modules.default_used_modules := List.assoc set default_used_interfaces
  with Not_found ->
    failwith ("unknown module set " ^ set)
and log_mode () =
  verbose := true
and no_warnings() =
  warn_flag := false
;;

let main() =
try
  Sys.catch_break true;
  Modules.default_used_modules := List.assoc "cautious" default_used_interfaces;
  load_path := [];
  Caml__csl.arg_parse ["-clib", Arg.String set_zlc;
              "-o", Arg.String set_output;
              "-c", Arg.String merge_library;
              "-I", Arg.String add_include;
              "-include", Arg.String add_include;
              "-O", Arg.String open_set;
              "-open", Arg.String open_set;
              "-L", Arg.Unit log_mode;
              "-W", Arg.Unit no_warnings;
              "-", Arg.String anonymous]
             anonymous;
  exit 0
with Failure s -> prerr_endline ("Fatal error: " ^ s ^ "."); exit 2
   | Sys.Break -> exit 2
   | Sys_error msg ->
      prerr_endline ("Input/output error: " ^ msg ^ ".");
      exit 2
   | e -> prerr_endline "Unexpected exception. Please report."; raise e
;;

Printexc.catch main ()
