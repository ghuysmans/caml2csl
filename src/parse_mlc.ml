
#open "globals";;
#open "syntax";;
#open "modules";;
#open "conv";;
#open "enter";;
#open "genlex";;



let lexer=make_lexer ["{";"}";"[";"]";"COPY";
           "MODULE";"OPEN";"IS";"VALUE";"TYPE";"CONSTR";"LABEL";"=";"."];;


let parse_id= function
   [< 'Ident id >] -> id
 | [< 'String s >] -> s
;;

let parse_new ol=function
   [< 'Kwd "=" >] -> {qual= !csl_def_mod; id= ol }
 | [< 'Kwd "."; parse_id nw >] -> {qual= !csl_def_mod; id= nw}
 | [< parse_id module; 'Kwd "." ; parse_id nw >] -> {qual= module; id= nw}
;;

let parse_ligne sel= function
   [< parse_id ol; (parse_new ol) qi >] -> enter_conv sel ol qi
;;

let parse_constr_ligne= function
   [< parse_id ol; (parse_new ol) qi; 'Int ar >]
               -> enter_conv (CONSTR ar) ol qi
;;

let rec parse_bloc sel= function
   [< 'Kwd "}" >] -> () 
 | [< (parse_ligne sel) _ ; (parse_bloc sel) _ >] -> ()
;;

let rec parse_constr_bloc= function
   [< 'Kwd "}" >] -> () 
 | [< parse_constr_ligne _ ; parse_constr_bloc _ >] -> ()
;;

let rec parse_cb= function
   [< 'Kwd "VALUE" ; 'Kwd "{" ; (parse_bloc (VAR [])) _ >] -> ()
 | [< 'Kwd "TYPE" ; 'Kwd "{" ; (parse_bloc TYP) _ >] -> ()
 | [< 'Kwd "CONSTR" ; 'Kwd "{" ; parse_constr_bloc _ >] -> ()
 | [< 'Kwd "LABEL" ; 'Kwd "{" ; (parse_bloc LABEL) _ >] -> ()
;;

let rec parse_mb= function
   [< 'Kwd "}" >] -> ()
 | [< parse_cb _; parse_mb _ >] -> ()
;;



let parse_opt_module md strm =
  defined_module := new_module md;
  hashtbl__add module_table md !defined_module;
  match strm with
   [< 'Kwd "IS"; parse_id nm >]
      -> csl_def_mod := nm
 | [< 'Kwd "OPEN"; parse_id nm >]
      -> hashtbl__add mod_name_table md nm;
         csl_def_mod := nm
 | [< >]
      -> let nm = (change_case upper md) in
           hashtbl__add mod_name_table md nm;
           csl_def_mod := nm
;;

let parse_mlc = function
   [< 'Kwd "MODULE"; parse_id md; (parse_opt_module md) _;
      'Kwd "{"; parse_mb _ >] -> ()
;;

let compile_mlc filename=
  let ch=open_in (filename ^ ".mlc") in
  parse_mlc (lexer (stream_of_channel ch));
  close_in ch
;;


let parse_module = function
   [< 'Kwd "COPY"; parse_id x; parse_id y >]
      -> hashtbl__add module_table y (hashtbl__find module_table x)
 | [< 'Kwd "MODULE"; parse_id md; (parse_opt_module md) _;
       'Kwd "{"; parse_mb _ >] -> ()
;;


let rec parse_file = function
   [< parse_module _; parse_file _ >] -> ()
 | [< >] -> ()
;;


let compile_stdfile filename =
  try
    let ch=open_in (find_in_path filename) in
      try 
        parse_file (lexer (stream_of_channel ch));
        close_in ch
      with
        Parse_error -> failwith ("Syntax error in '" ^ filename ^ "'")
  with Cannot_find_file _ -> failwith ("Cannot find library source '"
                                                      ^ filename ^ "'")
;;
