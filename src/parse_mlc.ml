
open Globals;;
open Syntax;;
open Modules;;
open Conv;;
open Enter;;
open Genlex;;



let lexer=make_lexer ["{";"}";"[";"]";"COPY";
           "MODULE";"OPEN";"IS";"VALUE";"TYPE";"CONSTR";"LABEL";"=";"."];;


let parse_id= parser
   [< 'Ident id >] -> id
 | [< 'String s >] -> s
;;

let parse_new ol=parser
   [< 'Kwd "=" >] -> {qual= !csl_def_mod; id= ol }
 | [< 'Kwd "."; nw = parse_id >] -> {qual= !csl_def_mod; id= nw}
 | [< module0 = parse_id; 'Kwd "." ; nw = parse_id >] -> {qual= module0; id= nw}
;;

let parse_ligne sel= parser
   [< ol = parse_id; qi = (parse_new ol) >] -> enter_conv sel ol qi
;;

let parse_constr_ligne= parser
   [< ol = parse_id; qi = (parse_new ol); 'Int ar >]
               -> enter_conv (CONSTR ar) ol qi
;;

let rec parse_bloc sel= parser
   [< 'Kwd "}" >] -> () 
 | [< _ = (parse_ligne sel) ; _ = (parse_bloc sel) >] -> ()
;;

let rec parse_constr_bloc= parser
   [< 'Kwd "}" >] -> () 
 | [< _ = parse_constr_ligne ; _ = parse_constr_bloc >] -> ()
;;

let rec parse_cb= parser
   [< 'Kwd "VALUE" ; 'Kwd "{" ; _ = (parse_bloc (VAR [])) >] -> ()
 | [< 'Kwd "TYPE" ; 'Kwd "{" ; _ = (parse_bloc TYP) >] -> ()
 | [< 'Kwd "CONSTR" ; 'Kwd "{" ; _ = parse_constr_bloc >] -> ()
 | [< 'Kwd "LABEL" ; 'Kwd "{" ; _ = (parse_bloc LABEL) >] -> ()
;;

let rec parse_mb= parser
   [< 'Kwd "}" >] -> ()
 | [< _ = parse_cb; _ = parse_mb >] -> ()
;;



let parse_opt_module md strm =
  defined_module := new_module md;
  Hashtbl.add module_table md !defined_module;
  match strm with parser
   [< 'Kwd "IS"; nm = parse_id >]
      -> csl_def_mod := nm
 | [< 'Kwd "OPEN"; nm = parse_id >]
      -> Hashtbl.add mod_name_table md nm;
         csl_def_mod := nm
 | [< >]
      -> let nm = (change_case upper md) in
           Hashtbl.add mod_name_table md nm;
           csl_def_mod := nm
;;

let parse_mlc = parser
   [< 'Kwd "MODULE"; md = parse_id; _ = (parse_opt_module md);
      'Kwd "{"; _ = parse_mb >] -> ()
;;

let compile_mlc filename=
  let ch=open_in (filename ^ ".mlc") in
  parse_mlc (lexer (Caml__csl.stream_of_channel ch));
  close_in ch
;;


let parse_module = parser
   [< 'Kwd "COPY"; x = parse_id; y = parse_id >]
      -> Hashtbl.add module_table y (Hashtbl.find module_table x)
 | [< 'Kwd "MODULE"; md = parse_id; _ = (parse_opt_module md);
       'Kwd "{"; _ = parse_mb >] -> ()
;;


let rec parse_file = parser
   [< _ = parse_module; _ = parse_file >] -> ()
 | [< >] -> ()
;;


let compile_stdfile filename =
  try
    let ch=open_in (find_in_path filename) in
      try 
        parse_file (lexer (Caml__csl.stream_of_channel ch));
        close_in ch
      with
        (Stream.Error "") -> failwith ("Syntax error in '" ^ filename ^ "'")
  with Cannot_find_file _ -> failwith ("Cannot find library source '"
                                                      ^ filename ^ "'")
;;
