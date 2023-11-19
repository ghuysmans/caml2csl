open Globals;;
open Syntax;;
open Modules;;
open Hashtbl;;
open Lexer;;

let print_tbl h=
  iter (fun s {qual=m;id=n} -> prerr_endline (s^"=>"^m^" . "^n)) h
;;

let print_ar h=
  iter (fun s ar -> prerr_endline (s^"->"^(string_of_int ar))) h
;;

let print_module md=
  prerr_endline ("#"^md.mod_name^":");
  print_tbl md.mod_types;
  print_tbl md.mod_values;
  print_tbl md.mod_constrs;
  print_tbl md.mod_labels;
  print_ar md.mod_arity
;;


let print_zc file=
  let f=open_in_bin file in
  let (md,cmd)=(input_value f:(module0*string)) in
  print_module md;
  prerr_endline ("==>" ^ cmd);
  close_in f
;;




let output_qual_id eqmn ch s qi =
  output_string ch (s^"\t");
  if eqmn = Some qi.qual
  then if s = qi.id then output_string ch "="
       else output_string ch ("."^qi.id)
  else output_string ch (qi.qual^"."^qi.id)
;;

let output_constr eqmn ch h har=
  iter (fun s qi -> output_qual_id eqmn ch s qi;
                        output_string ch ("\t"^(string_of_int (find har s))^"\n")) h
;;

let output_tbl eqmn ch h=
  iter (fun s qi -> output_qual_id eqmn ch s qi; output_string ch "\n") h
;;

let output_module eqmn md ch =
  output_string ch "TYPE {\n";
  output_tbl eqmn ch md.mod_types;
  output_string ch "}\nVALUE {\n";
  output_tbl eqmn ch md.mod_values;
  output_string ch "}\nCONSTR {\n";
  output_constr eqmn ch md.mod_constrs md.mod_arity;
  output_string ch "}\nLABEL {\n";
  output_tbl eqmn ch md.mod_labels;
  output_string ch "}\n"
;;


let dump_lib (mtbl, csl_mtbl, nm_tbl) ch =
(*  output_string ch "CAML-LIGHT MODULES:\n\n\n";*)
  iter (fun s md -> output_string ch ("MODULE "^s);
              let equiv = (try let op_equiv = Hashtbl.find nm_tbl s in
                    output_string ch (" OPEN "^op_equiv^" {\n"); Some op_equiv
              with Not_found -> output_string ch " {\n"; None ) in
              output_module equiv md ch;
              output_string ch "}\n\n\n") mtbl
(*  output_string ch "CSL MODULES:\n\n\n";
  do_table (fun s md -> output_string ch ("Module "^s^"\n"); output_module md ch) csl_mtbl*)
;;

let pp_lib clib ofi =
  let oc = open_out ofi in
  let ic = try open_in_bin (find_in_path clib)
           with Cannot_find_file _
                    -> failwith ("Cannot find library '" ^ clib ^ "'")
   in
  try
    let st = (input_value ic: (string,module0) Hashtbl.t
                               * (string,module0) Hashtbl.t
                                * (string,string) Hashtbl.t) in
    close_in ic;
    dump_lib st oc;
    close_out oc
  with End_of_file | Failure _ ->
    close_in ic;
    close_out oc;
    failwith "Corrupted standard conversion file"
;;

(*pp_lib "coqV6.zlc" "coq.mlc";;*)
(*
do_table (fun s md -> print_endline ("******"^s); print_module md) module_table;;
do_table (fun s md -> print_endline ("******"^s); print_module md) csl_module_table;;

do_table (fun s md -> print_endline ("******"^s)) module_table;;
do_table (fun s md -> print_endline ("******"^s)) csl_module_table;;
do_table (fun s s' -> print_endline (s^"=>"^s')) mod_name_table;;
*)
(*    let ch= open_in_bin (filename ^ ".zc") in
    let (md,cmd)=(input_value f:(module*module)) in
    hashtbl__add module_table md.mod_name md;
    hashtbl__add csl_module_table cmd.mod_name cmd;
    hashtbl__add mod_name_table md.mod_name cmd.mod_name; 
    close_in ch;
*)


let merge_libs libname_list output_lib =
  reset_infix();
  all_reset();
  List.iter load_core_lib libname_list;
  write_core_lib output_lib
;;
(* merge_libs "std.zlc" "coqV6.zlc" "coqV6.zlc" "coq.mlc";;*)
