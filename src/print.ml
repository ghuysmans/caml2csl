
open Location;;
open Syntax;;
open Modules;;

let string_of_loc (Loc (d,f))=
 "(" ^ (string_of_int d) ^ "," ^ (string_of_int f) ^ ")"
;;

let string_of_gr= function
   GRname s -> s
 | GRmodname q -> q.qual^"."^q.id
;;

let string_of_gi=function
  GImodname (qi,_,_,_,_) -> qi.qual ^ "__" ^ qi.id
| GIname ((s,_),_) -> s
;;



let print_gi gi = prerr_endline (string_of_gi gi);;

let print_tbl h=
  Hashtbl.iter
     (fun s qi -> prerr_endline (s ^ "-->" ^ (string_of_gr (GRmodname qi)))) h
;;

let print_ar h=
  Hashtbl.iter (fun s ar -> prerr_endline (s^"->"^(string_of_int ar))) h
;;

let print_module md=
  prerr_endline ("Module "^md.mod_name^":\n>> TYPES:");
  print_tbl md.mod_types;
  prerr_endline ">> VALUES:";
  print_tbl md.mod_values;
  prerr_endline ">> CONSTRS:";
  print_tbl md.mod_constrs;
  prerr_endline ">> LABELS:";
  print_tbl md.mod_labels;
  prerr_endline ">> ARITY:";
  print_ar md.mod_arity
;;


let print_zc file=
  let f=open_in_bin file in
  let (md,cmd)=(input_value f:(module0*string)) in
  print_module md;
  prerr_endline ("==> OPEN: " ^ cmd);
  close_in f
;;
