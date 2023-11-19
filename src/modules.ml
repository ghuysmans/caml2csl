open Globals;;
open Syntax;;

(* Informations associated with module names *)

type module0 =
  { mod_name: string;                        (* name of the module *)
    mod_values: (string, qualified_ident) Hashtbl.t;
                                             (* table of values *)
    mod_constrs: (string, qualified_ident) Hashtbl.t;
                                             (* table of constructors *)
    mod_labels: (string, qualified_ident) Hashtbl.t;
                                             (* table of labels *)
    mod_types: (string, qualified_ident) Hashtbl.t;
                                             (* table of type constructors *)
    mod_arity: (string, int) Hashtbl.t
                                             (* arity of constructors *)
  }
;;


let name_of_module     md = md.mod_name
and values_of_module   md = md.mod_values
and constrs_of_module  md = md.mod_constrs
and labels_of_module   md = md.mod_labels
and types_of_module    md = md.mod_types
and arities_of_module  md = md.mod_arity
;;

(* The table of module interfaces already loaded in memory *)

let module_table = (Hashtbl.create 37 : (string, module0) Hashtbl.t);;
let csl_module_table = (Hashtbl.create 37 : (string, module0) Hashtbl.t);;

(* The table of module name conversions *)
let mod_name_table = (Hashtbl.create 37 : (string,string) Hashtbl.t);;


let all_reset() =
  Hashtbl.clear module_table;
  Hashtbl.clear csl_module_table;
  Hashtbl.clear mod_name_table
;;

let merge_state (mt,cmt,mnt) =
  add_table mt module_table;
  add_table cmt csl_module_table;
  add_table mnt mod_name_table    
;;

let load_core_lib libname=
  let ic = try open_in_bin (find_in_path libname)
           with Cannot_find_file _
                    -> failwith ("Cannot find library '" ^ libname ^ "'")
   in
  try
    let st = (input_value ic: (string,module0) Hashtbl.t
                               * (string,module0) Hashtbl.t
                                * (string,string) Hashtbl.t) in
    close_in ic;
    merge_state st
  with End_of_file | Failure _ ->
    close_in ic;
    failwith "Corrupted standard conversion file"
;;

let write_core_lib libname =
  try
    let f=open_out_bin libname in
    try
      output_value f (module_table,csl_module_table,mod_name_table);
      close_out f
    with Sys_error _
           -> Sys.remove libname;
              failwith ("Error in writing standard conversion file '"
                            ^ libname ^ "'")
  with Sys_error _ -> failwith ("Cannot open '" ^ libname ^ "'")
;;


let new_module nm=
    { mod_name = nm;
      mod_values = Hashtbl.create 17;
      mod_constrs = Hashtbl.create 13;
      mod_labels = Hashtbl.create 11;
      mod_types = Hashtbl.create 7;
      mod_arity = Hashtbl.create 13 }
;;

let read_module filename =
  let ic = open_in_bin filename in
  try
    let zc_obj = (input_value ic : module0*string) in
    close_in ic;
    zc_obj
  with End_of_file | Failure _ ->
    close_in ic;
    failwith ("Corrupted compiled conversion file "^filename)
;;

let load_module name =
  try
    let fullname = find_in_path (name ^ ".zc") in
    read_module fullname
  with Cannot_find_file _ ->
    failwith ("Cannot find the compiled conversion file "^name^".zc")
;;

(* To find an interface by its name *)

let find_csl_module modname =
  try
    Hashtbl.find csl_module_table modname
  with Not_found -> let cmd=new_module modname in
                     Hashtbl.add csl_module_table modname cmd;
                     cmd
;;

let add_csl_info sel_fct old qi=
  table_add (sel_fct (find_csl_module qi.qual)) qi.id qi
;;

let find_module modname =
  try
    Hashtbl.find module_table modname
  with Not_found ->
    let (md,equiv) = load_module modname in
      Hashtbl.add module_table modname md;
      Caml__csl.do_table_rev (add_csl_info values_of_module) md.mod_values;
      Caml__csl.do_table_rev (add_csl_info types_of_module) md.mod_types;
      Caml__csl.do_table_rev (add_csl_info constrs_of_module) md.mod_constrs;
      Caml__csl.do_table_rev (add_csl_info labels_of_module) md.mod_labels;
      Hashtbl.add mod_name_table modname equiv;
      md
;;

(* The table of all opened modules. Associate to each unqualified name
   the corresponding descriptor from the right opened module. *)

let opened_modules = ref
  { mod_name = "";
    mod_values = Hashtbl.create 73;
    mod_constrs = Hashtbl.create 53;
    mod_labels = Hashtbl.create 41;
    mod_types = Hashtbl.create 29;
    mod_arity = Hashtbl.create 53 }

;;

let csl_opened_modules = ref
  { mod_name = "";
    mod_values = Hashtbl.create 73;
    mod_constrs = Hashtbl.create 53;
    mod_labels = Hashtbl.create 41;
    mod_types = Hashtbl.create 29;
    mod_arity = Hashtbl.create 53 }
;;

let opened_modules_names = ref ([]: string list);;


let reset_opened_modules () =
  opened_modules :=
    { mod_name = "";
      mod_values = Hashtbl.create 73;
      mod_constrs = Hashtbl.create 53;
      mod_labels = Hashtbl.create 41;
      mod_types = Hashtbl.create 29;
      mod_arity = Hashtbl.create 53 };
  opened_modules_names := []
;;

let reset_csl_opened_modules () =
  csl_opened_modules :=
    { mod_name = "";
      mod_values = Hashtbl.create 73;
      mod_constrs = Hashtbl.create 53;
      mod_labels = Hashtbl.create 41;
      mod_types = Hashtbl.create 29;
      mod_arity = Hashtbl.create 53 }
;;


(* Open a module and add its definitions to the table of opened modules. *)

let open_module name =
  let module0 = find_module name in
  add_table module0.mod_values !opened_modules.mod_values;
  add_table module0.mod_constrs !opened_modules.mod_constrs;
  add_table module0.mod_labels !opened_modules.mod_labels;
  add_table module0.mod_types !opened_modules.mod_types;
  add_table module0.mod_arity !opened_modules.mod_arity;
  opened_modules_names := name :: !opened_modules_names
;;

let open_csl_module name=
  let module0 = find_csl_module name in
    add_table module0.mod_values !csl_opened_modules.mod_values;
    add_table module0.mod_types !csl_opened_modules.mod_types;
    add_table module0.mod_constrs !csl_opened_modules.mod_constrs;
    add_table module0.mod_labels !csl_opened_modules.mod_labels
;;


(* Close a module and remove its definitions from the table of opened modules.
   To avoid heavy hashtbl hacking, we just rebuild the table from scratch.
   Inefficient, but #close is not frequently used. *)

let close_module name =
  let other_modules_names = Caml__csl.except name !opened_modules_names in
  reset_opened_modules();
  List.iter open_module (List.rev other_modules_names);;


(* The current state of the compiler *)

let default_used_modules = ref ([] : string list);;

let defined_module = ref (new_module "");;
let csl_def_mod = ref "";;
let conv_hints = ref (new_module "");;


(* Reset to initial state of module tables *)

let reset_bare() =
  all_reset();
  reset_opened_modules();
  reset_csl_opened_modules()
;;
  
let reset_modules () =
  reset_bare();
  if !core_lib <> "" then 
   begin
    load_core_lib !core_lib;
    List.iter open_module !default_used_modules;
    open_csl_module "Pervasives";
    open_csl_module "__"
   end
;;


(* Addition to the defined module *)

let add_info sel_fct name info =
  table_add (sel_fct !defined_module) name info
;;

let add_csl_open sel_fct old qi =
  Hashtbl.add (sel_fct !csl_opened_modules) qi.id qi
;; 

(* caml2csl will work on .ml files even if type decls are not yet copied
  (assumes that they are at the beginning of the file) *)
let init_defined_module modname conv_mod=
 let module0 = { mod_name = modname;
                mod_values = Hashtbl.create 17;
                mod_constrs = conv_mod.mod_constrs;
                mod_labels = conv_mod.mod_labels;
                mod_types = conv_mod.mod_types;
                mod_arity = conv_mod.mod_arity } in
  defined_module := module0;
  Hashtbl.add module_table modname module0;
  Caml__csl.do_table_rev (add_csl_open constrs_of_module) module0.mod_constrs;
  Caml__csl.do_table_rev (add_csl_open labels_of_module) module0.mod_labels;
  Caml__csl.do_table_rev (add_csl_open types_of_module) module0.mod_types
;;

(* start_compiling an interface or an implementation without interface *)
let start_compiling modname=
  let md = new_module modname in
  let new_name = change_case upper modname in
   inform (modname ^ " --> " ^ new_name);
   Hashtbl.add module_table modname md;
   Hashtbl.add mod_name_table modname new_name;
   defined_module := md;
   csl_def_mod := new_name;
   conv_hints := new_module ""
;;


let conv_module_name md =
  try Hashtbl.find mod_name_table md
  with Not_found -> inform ("No equivalent for module " ^ md); ""
;;

(* start compiling implementation: assumes that the .camli is already 
   compiled *)
let start_compiling_impl modname=
  let md = find_module modname in
   init_defined_module modname md;
   conv_hints := md;
   csl_def_mod := conv_module_name modname
;;

(* To write the interface of the module currently compiled *)
let write_conv_tab filename=
  try
    let f=open_out_bin (filename^".zc") in
    try
      output_value f (!defined_module,!csl_def_mod);
      close_out f
    with Sys_error _
           -> Sys.remove (filename ^ ".zc");
              failwith ("Error in writing compiled conversion file '"
                            ^ filename ^ ".zc'")
  with Sys_error _ -> failwith ("Cannot open '" ^ filename ^ ".zc'")
;;



(* Find the descriptor for a reference to a global identifier.
   If the identifier is qualified (mod__name), just look into module mod.
   If the identifier is not qualified, look inside the current module,
   then inside the table of opened modules. *)

let find_global_desc sel_fct qi=
    Hashtbl.find (sel_fct (find_module qi.qual)) qi.id
;;

let find_local_desc sel_fct s=
  try Hashtbl.find (sel_fct !defined_module) s
  with Not_found -> Hashtbl.find (sel_fct !opened_modules) s
;;


(* The same for arity of constructors *)

let find_arity_global qi=
  try Hashtbl.find (find_module qi.qual).mod_arity qi.id
  with Not_found -> failwith ("find_arity_global: "^qi.qual^"."^qi.id)
;;

let find_arity_local s=
  try Hashtbl.find !defined_module.mod_arity s
  with Not_found ->
    try Hashtbl.find !opened_modules.mod_arity s
    with Not_found -> failwith ("find_arity_local: "^s)
;;

let arity_of= function
  GIname ((s,_),_) -> find_arity_local s
| GImodname (qi,_,_,_,_) -> find_arity_global qi
;;
 
