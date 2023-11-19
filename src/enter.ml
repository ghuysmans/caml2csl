open Globals;;
open Syntax;;
open Modules;;
open Emit;;
open Conv;;
open Changes;;


(* global name conversions *)

let extend g x=
  let s= change_case (case_of_genre g) x in
  let rec try_succ n=
    let name = ext_id s n in
    if (used_csl_unqual_id (sel_of_genre g) name)
     then try_succ (succ n)
     else name in
  if used_csl_unqual_id (sel_of_genre g) s then try_succ 0 else s
;;

let new_defined g x=
  try (Hashtbl.find (sel_of_genre g !defined_module) x).id
                                        (* preservation des captures *)
  with Not_found ->
   (try (Hashtbl.find (sel_of_genre g !conv_hints) x).id
    with Not_found ->
      (try (Hashtbl.find (sel_of_genre g !opened_modules) x).id
       with Not_found -> extend g x))
;;


let enter_conv g s qi=
  let sel_fct= sel_of_genre g in
    add_info sel_fct s qi;
    add_csl_info sel_fct s qi;
    add_csl_open sel_fct s qi;
    match g with
      CONSTR ar -> add_info arities_of_module s ar
    | _ -> ()
;;

let enter_def_conv g s s' =
  enter_conv g s {qual= !csl_def_mod; id=s'}
;;

let enter_new_conv g x =
  let new0=new_defined g x in
  enter_def_conv g x new0
;;


(* implementation and interface phrases *)

let arity_of_type= function
  Ztypetuple l -> List.length l
| _ -> 1
;;


let enter_new_constr env= function
    Zconstr0decl (c, loc) -> enter_new_conv (CONSTR 0) c;
                             chg_local_ident 0 (CONSTR 0) (c,loc)
  | Zconstr1decl ((c, loc), arg, mut_flag) ->
      if mut_flag=Mutable then todo ("mutable constructor: " ^ c);
      enter_new_conv (CONSTR (arity_of_type arg.te_desc)) c;
      chg_local_ident 0 (CONSTR 0) (c,loc);
      chg_typ_expr true env arg
;;


let enter_new_label env ((label,loc),typ,_)=
  enter_new_conv LABEL label;
  chg_local_ident 0 LABEL (label,loc);
  chg_typ_expr false env typ
;;



let define_new_type_intf name env= function
   Zabstract_type -> ()
 | Zvariant_type constrs ->
      List.iter (enter_new_constr env) constrs;
      todo ("copy type declaration of " ^ name)
 | Zrecord_type labels ->
      List.iter (enter_new_label env) labels;
      todo ("copy type declaration of " ^ name)
 | Zabbrev_type (eqloc,body) ->
      emit_chg eqloc "=";
      chg_typ_expr false env body;
      todo ("copy type declaration of " ^ name)
;;

let enter_typedecl_intf decl loc = 
  List.iter
    (fun ((t,_),_,_) -> enter_new_conv TYP t) decl;
  List.iter (fun ((t,loct),vars,def) ->
          let env=List.fold_left add_local_var [] (fst (List.split vars)) in
           List.iter (chg_local_ident 0 (VAR env)) vars;
           chg_local_ident 0 TYP (t,loct);
           define_new_type_intf t env def) decl
;;

let enter_excdecl_intf decl loc=
  List.iter (fun (andloc,exc) ->
      emit_chg andloc "exception";
      enter_new_constr [] exc) decl;
  todo ("copy exception declaration of " ^ (mk_list ", " (List.map
     (function _, Zconstr0decl (s,_) -> s 
             | _, Zconstr1decl ((s,_),_,_) -> s)
    decl)))
;;

let enter_valuedecl decl loc =
  let conv_val (loc1,(((s,_),_) as lid ,expr,prim)) =
     enter_new_conv (VAR []) s;
     if prim = None then emit_chg loc1 "val"
      else emit_chg loc1 "external";
     chg_ident 0 (VAR []) (GIname lid);
     chg_typ_expr false [] expr;
     match prim with
       Some l -> emit_chg l "";
                 todo ("copy primitive " ^ s)
     | None -> ()  in
  List.iter conv_val decl
;;



let define_new_type name env= function
   Zabstract_type -> ()
 | Zvariant_type constrs -> List.iter (enter_new_constr env) constrs
 | Zrecord_type labels -> List.iter (enter_new_label env) labels
 | Zabbrev_type (eqloc,body) -> emit_chg eqloc "="; chg_typ_expr false env body
;;


let enter_typedecl decl = 
  List.iter (fun ((t,_),_,_) -> enter_new_conv TYP t) decl;
  List.iter (fun ((t,loc),vars,def) ->
          let env=List.fold_left add_local_var [] (fst (List.split vars)) in
           List.iter (chg_local_ident 0 (VAR env)) vars;
           chg_local_ident 0 TYP (t,loc);
           define_new_type t env def) decl
;;

let enter_excdecl decl= List.iter (fun (andloc,exc) ->
      emit_chg andloc "exception";
      enter_new_constr [] exc) decl;;


let enter_letdef (rec_flag,pat_expr_list) =
  let decl_names = conv_pat_list 0 [] (fst (List.split pat_expr_list)) in
  if rec_flag then begin
    List.iter (function (x, ID new0) -> enter_def_conv (VAR []) x new0
                    | (x, _) -> todo
                   (x ^ " defined with constructor of arity greater than 1."))
            decl_names;
    List.iter (fun (pat,expr) ->
             chg_pat decl_names pat;
             chg_expr 0 [] expr) pat_expr_list
  end else begin
    List.iter (fun (pat,expr) ->
             chg_pat decl_names pat;
             chg_expr 0 [] expr) pat_expr_list;
    List.iter (function (x, ID new0) -> enter_def_conv (VAR []) x new0
                    | (x, _) -> todo
                   (x ^ " defined with constructor of arity greater than 1."))
            decl_names
  end
;;

