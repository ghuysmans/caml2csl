open Print;;
open Globals;;
open Lexer;;
open Location;;
open Syntax;;
open Modules;;
open Emit;;
open Changes;;
open Enter;;
open Parse_mlc;;


(* Parsing functions *)

let parse_phrase parsing_fun lexing_fun lexbuf =
  try
    parsing_fun lexing_fun lexbuf
  with Parsing.Parse_error ->
        let pos1 = Lexing.lexeme_start lexbuf in
        let pos2 = Lexing.lexeme_end lexbuf in
        failwith ("Syntax error at " ^ (string_of_loc (Loc (pos1,pos2))))
     | Lexer.Lexical_error(errcode, pos1, pos2) ->
        let l = Loc(pos1, pos2) in
        failwith ("Lexical error at " ^ (string_of_loc l))
;;

let parse_impl_phrase = parse_phrase Parser.implementation Lexer.main
and parse_intf_phrase = parse_phrase Parser.interface Lexer.main
;;

(* Executing directives *)

let do_directive loc = function
    Zdir("open", (name,l)) ->
      open_module name;
      let new0 = conv_module_name name in
       if new0 = "" or name = !defined_module.mod_name then emit_chg loc ""
        else begin
          open_csl_module new0;
          emit_chg loc ("open "^new0^";;")
        end 
  | Zdir("close", (name,l)) ->
      close_module name;
      emit_chg loc ""
  | Zdir("infix", (name,l)) ->
      emit_chg loc "";
      add_infix name
  | Zdir("uninfix", (name,l)) ->
      emit_chg loc "";
      remove_infix name
  | Zdir("directory", (dirname,_)) ->
      load_path := dirname :: !load_path
  | Zdir(d, (name,_)) ->
      warn ("unknown directive \"#" ^ d ^ "\", ignored")
;;


(* Compiling a phrase *)

let conv_intf_phrase phr =
  match phr.in_desc with
    Zvaluedecl decl -> enter_valuedecl decl phr.in_loc
  | Ztypedecl decl -> enter_typedecl_intf decl phr.in_loc
  | Zexcdecl decl -> enter_excdecl_intf decl phr.in_loc
  | Zintfdirective dir -> do_directive phr.in_loc dir
;;

let conv_impl_phrase phr =
  begin match phr.im_desc with
    Zexpr expr -> chg_expr 0 [] expr
  | Zletdef (x0,x1) -> enter_letdef (x0,x1)
  | Ztypedef decl -> enter_typedecl decl
  | Zexcdef decl -> enter_excdecl decl
  | Zimpldirective dir -> do_directive phr.im_loc dir
  end
;;


(* Compiling an implementation and interface *)

let conv_interface filename =
  let source_name = filename ^ ".camli"
  and csl_file = (Filename.concat (Filename.dirname filename)
                       (change_case lower !csl_def_mod)) ^ ".mli" in
  let ic = open_in source_name in
  begin_chg source_name csl_file;
  let lexbuf = Lexing.from_channel ic in
    try
      while true do
         conv_intf_phrase (parse_intf_phrase lexbuf);
         flush !o_f
      done
    with End_of_file ->
      close_in ic;
      end_chg()
    | x ->
      if (not (!verbose)) then Sys.remove csl_file;
      close_in ic;
      raise x
;;

let conv_impl filename =
  let source_name = filename ^ ".caml"
  and csl_file = (Filename.concat (Filename.dirname filename)
                       (change_case lower !csl_def_mod)) ^ ".ml" in
  let ic = open_in source_name in
  begin_chg source_name csl_file;
  let lexbuf = Lexing.from_channel ic in
    try
      while true do
        conv_impl_phrase (parse_impl_phrase lexbuf);
        flush !o_f
      done
    with End_of_file ->
      close_in ic;
      end_chg()
    | x ->
      if (not (!verbose)) then Sys.remove csl_file;
      close_in ic;
      raise x
;;


let init_with_mlc modname filename =
  if file_exists (filename ^ ".mlc") then begin
    prerr_endline ("Compiling " ^ filename ^ ".mlc...");
    compile_mlc filename
  end else
    start_compiling modname
;;


let convert_mlc modname filename =
  compile_mlc filename;
  write_conv_tab filename
;;


let convert_interface modname filename =
  init_with_mlc modname filename;
  conv_interface filename;
  write_conv_tab filename
;;


let convert_implementation modname filename =
  if file_exists (filename ^ ".camli") then begin
    if (not (file_exists (filename ^ ".zc"))) then
      failwith ("Convert " ^ filename ^ ".camli first");
    start_compiling_impl modname;
    conv_impl filename
  end else begin
    init_with_mlc modname filename;
    conv_impl filename;
    write_conv_tab filename
  end
;;



let merge_library filename =
  reset_modules();
  if Filename.check_suffix filename ".zlc"
  then load_core_lib filename
  else compile_stdfile filename;
  write_core_lib !output_lib
;;


let extensions ext_act f=
  let rec ext_rec = function
    [] -> failwith ("Don't know what to do with " ^ f)
  | (ext,act)::rest -> 
      if Filename.check_suffix f ext
       then let file = Filename.chop_suffix f ext in
             act (Filename.basename file) file
       else ext_rec rest
  in ext_rec ext_act
;;

let anonymous file =
  reset_infix();
  reset_modules();
  uses_conv := false;
  if (not (file_exists file)) then failwith ("Cannot find '" ^ file ^ "'");
  extensions 
    [ ".caml", convert_implementation;
      ".camli", convert_interface;
      ".mlc", convert_mlc ]
    file;
  if !uses_conv then warn "uses module of conversion"
;;
