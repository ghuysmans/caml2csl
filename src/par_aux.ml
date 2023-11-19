(* Auxiliary functions for parsing *)

open Location;;
open Syntax;;
open Modules;;


let def_gi s= GIname ((s,(get_current_location())),None);;


let make_expr_chg desc chg=
  {e_desc = desc; e_chg = chg; e_loc= get_current_location() }
and make_pat_chg desc chg=
  {p_desc = desc; p_chg = chg; p_loc= get_current_location() }
;;

let make_typ desc=
  {te_desc = desc; te_loc = get_current_location() }
and make_impl desc=
  {im_desc = desc; im_loc = get_current_location() }
and make_intf desc=
  {in_desc = desc; in_loc = get_current_location() }
;;

let make_expr desc= make_expr_chg desc NO_CHANGE
and make_pat desc= make_pat_chg desc NO_CHANGE
;;


let make_apply chg_desc= function
    {e_desc = Zconstruct0(cstr1); e_chg=chg}, [e2] ->
      let (bef,bet,aft)= (match chg_desc with
			   NO_CHANGE -> NO_CHANGE, NO_CHANGE, NO_CHANGE
                         | SEQ [a;b;c] -> a, b, c
                         | x -> x, NO_CHANGE, NO_CHANGE )
      and (beg,en)= (match chg with
		        NO_CHANGE -> NO_CHANGE, NO_CHANGE
                      | SEQ [a;b] -> a, b
                      | x -> x, NO_CHANGE ) in
        make_expr_chg (Zconstruct1(cstr1, e2))
                       (SEQ [(SEQ [bef;beg]);(SEQ [en;bet]);aft])
  | e1, el ->
      make_expr_chg (Zapply(e1,el)) chg_desc
;;

let make_unop op e1 desc=
  make_apply desc ({e_desc = Zident(ref (Zlocal (op,None)));
              e_chg=NO_CHANGE; e_loc= (snd op) },[e1])
and make_binop op e1 e2 desc=
  make_apply desc ({e_desc = Zident(ref (Zlocal (op,None)));
              e_chg=NO_CHANGE; e_loc= (snd op) }, [e1;e2])
and make_ternop op e1 e2 e3 desc=
  make_apply desc ({e_desc = Zident(ref (Zlocal (op,None)));
              e_chg=NO_CHANGE; e_loc= (snd op) },[e1;e2;e3])
and make_infix op e1 e2 desc=
  make_apply desc (e1, [{e_desc = Zident(ref (Zlocal (op,None)));
              e_chg=NO_CHANGE; e_loc= (snd op) }; e2])
;;


let make_list expr_list os =
  
 let rec makel res = function
    [] ->
      res
  | e::l ->
      let cons_arg= make_expr(Ztuple [e;res]) in
      makel (make_expr (Zconstruct1((def_gi "::"),cons_arg))) l
 in makel (make_expr_chg(Zconstruct0 (def_gi "[]")) os) expr_list
  
;;






let chg_spec_constr1 id_loc (Loc (deb,fin))= function
   {qual="Pervasives"; id="ref"}
     -> SEQ [(REPLACE ((Loc (deb,deb)),"{"));
             (REPLACE (id_loc,"contents = "));
             (REPLACE ((Loc (fin,fin)),"}"))]
 | _ -> NO_CHANGE
;;

let special_pat_constr1 loc_tot= function
   GIname ((s,loc),_) ->
      chg_spec_constr1 loc loc_tot (find_local_desc constrs_of_module s)
 | GImodname (q,_,_,_,loc) ->
      chg_spec_constr1 loc loc_tot (find_global_desc constrs_of_module q)
;;


let special_constr (Loc (deb,fin))=function
   {qual="Stream"; id="Parse_error"}
      -> SEQ[(REPLACE ((Loc (deb,deb)),"("));
             (REPLACE ((Loc (fin,fin))," \"\")"))]
 | qi -> NO_CHANGE
;;


let expr_constr_or_ident= function
    GIname ((s,l),pref) as gr ->
      begin try
        let qi= find_local_desc constrs_of_module s in
        make_expr_chg(Zconstruct0 gr) (special_constr l qi)
      with Not_found ->
        make_expr(Zident(ref(Zlocal ((s,l),pref))))
      end
  | GImodname (q,(Loc (deb,_)),_,_,(Loc(_,fin))) as gr ->
     try
       let qi= find_global_desc constrs_of_module q in
         make_expr_chg (Zconstruct0 gr) (special_constr (Loc (deb,fin)) qi)
      with Not_found ->
       make_expr (Zident(ref(Zglobal gr)))
;;


let pat_constr_or_var ((s,loc),prfx) =
  try
    let qi= find_local_desc constrs_of_module s in
      make_pat_chg (Zconstruct0pat (GIname ((s,loc),prfx)))
                          (special_constr loc qi)
  with Not_found ->
    make_pat(Zvarpat ((s,loc),prfx))
;;

