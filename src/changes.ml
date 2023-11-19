open Print;;
open Globals;;
open Location;;
open Syntax;;
open Modules;;
open Emit;;
open Conv;;


(* Check if module qualification is necessary. *)

let simpl_no_capture g qi=
  try let q = Hashtbl.find (sel_of_genre g !csl_opened_modules) qi.id in
       if q = qi then (GRname qi.id)
       else if qi.qual <> !csl_def_mod then (GRmodname qi)
       else failwith ("local identifier " ^ qi.id ^ " was hidden when module "
      ^ q.qual ^ " was\n\t\topened. Please write a mlc file or open it later")
(* when the CSL module isn't opened: *)
  with Not_found -> GRmodname qi
;;

(* characters *)

let chg_char= function
   '\'' -> "'\''"
 | '`' -> "'`'"
 | c -> "'" ^ (Char.escaped c) ^ "'"
;;

let chg_atomic_cst= function
    ACother -> ()
  | ACchar (c,loc) -> emit_chg loc (chg_char c)
;;


(* name changes *)

exception Done;;

let remove_prefix = function
  Some loc -> emit_chg loc ""
| None -> ()
;;

let warn_special prfx loc = function
  {qual="Caml__csl"; id="combine"}
         -> warn ("list__combine in " ^ (string_of_loc loc))
(*| {qual="Pervasives"; id="not"}
         -> if List.mem "not" !Infix.list then begin
              remove_prefix prfx;
              raise Done
            end*)
| q -> ()
;;




let chg_modname m mloc m' dloc =
  if m' = "Caml__csl" then uses_conv := true;
  if m <> m' then emit_chg mloc m';
  emit_chg dloc "."
;;

let chg_prefix_id (Loc (deb,fin) as loc) afterdot s= fun
  p0 p1 -> match (p0,p1) with ((ID s'), None) -> if s<>s' then emit_chg loc s'
| ((LIST_ID ls), None) -> emit_chg loc ls
| ((ID s'), (Some pref_loc)) -> emit_chg pref_loc afterdot;
                   emit_chg loc ("("^s'^")")   (* attention a .( *)
| (_, _) -> failwith ("prefix cannot be a tuple")

;;


let check_var_arity arity loc = fun
  p0 p1 -> match (p0,p1) with ((VAR _), (ID _)) -> if arity > 1
                     then todo ("variable in " ^ (string_of_loc loc)
                        ^ " should be a " ^ (string_of_int arity) ^ "-uple")
| (_, _) -> ()
;;

(* en fait, csl_infix *)
let real_pref s prfx =
  if List.mem s !Infix.list then (remove_prefix prfx; None) else prfx
;;

let try_local_var arity = fun
  p0 p1 -> match (p0,p1) with ((VAR env), (GIname ((s,loc),prfx)))
     -> let str = List.assoc s env in
          check_var_arity arity loc (VAR env) str;
          chg_prefix_id loc "" s str (real_pref s prfx)
| (_, _) -> raise Not_found
;;

let try_ident arity g = function
   GIname ((s,loc),pref) ->
     let q = find_local_desc (sel_of_genre g) s in
       let (afterdot,id) = (match simpl_no_capture g q with
          GRname s' -> ("",s')
        | GRmodname qi ->
             if qi.qual = "__" then ("",qi.id)
             else let (Loc (deb,_))=loc in
                chg_modname "" (Loc (deb,deb)) qi.qual (Loc (deb,deb));
                (" ",qi.id) ) in
         warn_special pref loc q;
         chg_prefix_id loc afterdot s (ID id) (real_pref s pref)
 | GImodname (q,lm,ld,pref,lv) ->
     let qi = find_global_desc (sel_of_genre g) q in
       simpl_no_capture g qi;
       if qi.qual = "__" or qi.qual = !csl_def_mod then begin
         emit_chg lm "";
         emit_chg ld ""
       end else
         chg_modname q.qual lm qi.qual ld;
       warn_special pref lv qi;
       chg_prefix_id lv " " q.id (ID qi.id) (real_pref q.id pref)
;;

let chg_ident arity g gi =
  try try_local_var arity g gi
  with Not_found ->
    try check_var_arity arity (loc_of_gi gi) g (ID "");
        try_ident arity g gi
    with Not_found -> failwith ("chg_ident: "^(string_of_gi gi)^" not found")
       | Done -> ()
;;

let chg_local_ident arity g sloc =
  chg_ident arity g (GIname (sloc,None))
;;


(* streams *)

let rec check_stream loc = function
    (Znonterm _)::_::_ -> warn ("stream variable not last in "
                                 ^ (string_of_loc loc))
  | _::l -> check_stream loc l
  | [] -> ()
;;


(* pattern matrix *)

let new_pat (env,l) _ =
  let s=find_free_ext env "p" in
  (("",ID s)::env),(s::l)
;;

let add_comma_chg lp=
  let rec add_co= function
    p::(_::_ as t) -> let (Loc (_,f))=p.p_loc in
                        ((REPLACE ((Loc (f,f)),","))::(add_co t))
  | p -> let (Loc (_,f))=(List.hd p).p_loc in
            [REPLACE ((Loc (f,f)),")")]
  in let (Loc (deb,_))=(List.hd lp).p_loc in
   SEQ ((REPLACE ((Loc (deb,deb)),"("))::(add_co lp))
;;

let chg_fun loc_fun env= function
  ((_::_::_ as l1),_)::_::_ as l  (* pattern matrix *)
    -> let (env',lp)= List.fold_left new_pat (env,[]) l1 in
       let (Loc (deb,_))= (List.hd l1).p_loc in
        env',(SEQ 
            ((REPLACE ((Loc (deb,deb)),((mk_list " " lp) ^
                          " -> match (" ^ (mk_list "," lp)^") with ")))
             ::(List.map add_comma_chg (fst (List.split l)))))
| _::_::_ as l    (* pattern column *)
     -> env,SEQ ((REPLACE (loc_fun,"function"))::(List.map (fun _ -> NO_CHANGE) l))
| l -> env,NO_CHANGE
;;


let chg_constr0 (Loc (d,f)) qi =
  let arity = arity_of qi in
  let rec eta_expand = function
    1 -> "x_0"
  | n -> (eta_expand (pred n)) ^ "," ^ (ext_id "x" (pred n))
  in
  if arity > 0 then begin
    let ids = if arity = 1 then "x"
              else "(" ^ (eta_expand arity) ^ ")" in
      emit_chg (Loc (d,d)) ("(fun "^ids^" -> ");
      chg_ident 0 (CONSTR 0) qi;
      emit_chg (Loc (f,f)) (" "^ids^")")
  end else
    chg_ident 0 (CONSTR 0) qi
;;


(* type expressions *)

let chg_typ_expr cstr_flag env typ=
  let rec chg_te tf {te_desc=typ ; te_loc=loc }=match typ with
   Ztypevar (s,loc) -> let str=conv_local_var env s in
                       if s<>str then emit_chg loc str
 | Ztypearrow (a,b) -> if tf then 
                         let (Loc (d,f)) = loc in
                           emit_chg (Loc (d,d)) "(";
                           chg_te false a; chg_te false b;
                           emit_chg (Loc (f,f)) ")"
                       else
                         chg_te false a; chg_te false b
 | Ztypetuple l -> List.iter (chg_te false) l
 | Ztypeconstr (gi,l) -> List.iter (chg_te false) l; chg_ident 0 TYP gi
  in chg_te cstr_flag typ
;;


(* patterns *)

let rec chg_pat env pat=
  let chg_p= function
    Zwildpat -> do_chg
  | Zvarpat lid -> do_default (chg_ident 0 (VAR env)) [GIname lid]
  | Zaliaspat (p,sloc) -> do_frozen
       [(fun () -> chg_pat env p);
        (fun () -> chg_local_ident 0 (VAR env) sloc)]
  | Zconstantpat c -> do_default chg_atomic_cst [c]
  | Ztuplepat lp -> do_default (chg_pat env) lp
  | Zconstruct0pat gi -> do_default (chg_ident 0 (CONSTR 0)) [gi]
  | Zconstruct1pat (gi,p) -> do_frozen 
       [(fun () -> chg_ident 0 (CONSTR 0) gi);
        (fun () -> chg_pat env p)]
  | Zorpat (p1,p2) -> do_default (chg_pat env) [p1;p2]
  | Zconstraintpat (p,typ) -> do_frozen
       [(fun () -> chg_pat env p);
        (fun () -> chg_typ_expr false env typ)]
  | Zrecordpat l -> do_default (fun (lab,pat) ->
                         chg_ident 0 LABEL lab;
                         chg_pat env pat) l
  in chg_p pat.p_desc pat.p_chg
;;


(* expressions *)

let rec chg_expr ar env ex=
 let rec chg_e arity expr=
  let check_arity what =
    if arity > 1 then todo ("expression in " ^ (string_of_loc expr.e_loc)
                     ^ " should be a " ^ (string_of_int arity)
                     ^ "-uple\n\t\tinstead of " ^ what) in
  let chg_frzn= function
   Zident ({contents =  id}) -> do_default (chg_expr_ident arity) [id]
 | Zconstant c -> do_default chg_atomic_cst [c]
 | Ztuple l -> do_default (chg_e 0) l
 | Zconstruct0 qi -> do_default (chg_constr0 expr.e_loc) [qi]
 | Zconstruct1 (qi,e) -> do_frozen
       [(fun () -> chg_ident 0 (CONSTR 0) qi);
        (fun () -> chg_e (arity_of qi) e)] (* arite *)
 | Zapply (e,el) -> check_arity "an application"; do_default (chg_e 0) (e::el)
 | Zlet (b,dl,e)
     -> check_arity "a let expression";
        let ev=conv_pat_list 0 env (fst (List.split dl)) in
         do_frozen [(fun () -> List.iter (fun (pat,expr) ->
                                chg_pat ev pat;
                                chg_expr 0 (if b then ev else env) expr) dl) ;
                    (fun () -> chg_expr 0 ev e)]
 | Zfunction (opfun,l) -> 
       let ev,chgl=(match opfun with
                       Some loc_fun -> chg_fun loc_fun env l 
                     | None -> env,NO_CHANGE) in
       do_synchro (fun chg (pl,e) ->
            let evl=conv_pat_list 0 ev pl in
              do_default (chg_pat evl) pl chg;
              chg_expr 0 evl e) l chgl;
       do_chg               (* special *)
 | Ztrywith (e,l) -> check_arity "a try-with"; do_frozen
    ((fun  () -> chg_e 0 e)::
     (List.map (fun (p,ex) () -> let ev=conv_pat 0 env p in
                             chg_pat ev p;
                             chg_expr 0 ev ex) l))
 | Zsequence (e1,e2) -> check_arity "a sequence"; do_default (chg_e 0) [e1;e2]
 | Zcondition (e1,e2,e3) -> check_arity "a condition";
                            do_default (chg_e 0) [e1;e2;e3]
 | Zwhile (e1,e2) -> do_default (chg_e 0) [e1;e2]
 | Zfor ((((s,_),_) as lid),b1,b2,_,e)
      -> let ev = add_local_var env s in
          chg_ident 0 (VAR ev) (GIname lid);
          chg_e 0 b1;
          chg_e 0 b2;
          chg_expr 0 ev e;
          do_chg
 | Zsequand (e1,e2) -> do_default (chg_e 0) [e1;e2]
 | Zsequor (e1,e2) -> do_default (chg_e 0) [e1;e2]
 | Zconstraint (e,typ) -> check_arity "a type cast"; do_frozen 
     [(fun () -> chg_e 0 e);
      (fun () -> chg_typ_expr false env typ)]
 | Zvector el -> List.iter (chg_e 0) el; do_chg (* special *)
 | Zassign (lid,e) -> todo ("only record field may be assigned in "
                        ^ (string_of_loc expr.e_loc));
    do_frozen
     [(fun () -> chg_ident 0 (VAR env) (GIname lid));
      (fun () -> chg_e 0 e) ]
 | Zrecord l -> List.iter (fun (gi,e) -> chg_ident 0 LABEL gi;
                                       chg_e 0 e ) (List.rev l);
                do_chg (* special *)
 | Zrecord_access (e,lab) -> check_arity "a record access"; do_frozen
     [(fun () -> chg_e 0 e);
      (fun () -> chg_ident 0 LABEL lab) ]
 | Zrecord_update (e1,lab,e2) -> do_frozen
     [(fun () -> chg_e 0 e1);
      (fun () -> chg_ident 0 LABEL lab);
      (fun () -> chg_e 0 e2)]
 | Zstream l -> check_stream expr.e_loc l;
                List.iter chg_strm_cp l; do_chg  (* special *)
 | Zparser l -> do_synchro (fun chg (lp,e)
        -> let ev=List.fold_left conv_strm_pat env lp in
            List.iter (chg_strm_pat ev) lp;
            do_chg chg; chg_expr 0 ev e)  l
 | Zwhen (e1,e2) -> do_default (chg_e 0) [e1;e2]

  in chg_frzn expr.e_desc expr.e_chg

  and chg_expr_ident ar = function
   Zglobal gl -> chg_ident ar (VAR env)  gl
 | Zlocal lid -> chg_ident ar (VAR env) (GIname lid)

  and chg_strm_cp= function
   Zterm e -> chg_e 0 e
 | Znonterm e -> chg_e 0 e

  and chg_strm_pat env = function
   Ztermpat p -> chg_pat env p
 | Znontermpat (e,p) -> let (Loc (_,fin))=e.e_loc in
        do_swap e.e_loc p.p_loc (fun chg f -> f(); do_chg chg)
           [ NO_CHANGE; (REPLACE ((Loc (fin,fin))," =")); NO_CHANGE ]
           [(fun () -> chg_pat env p);
            (fun () -> chg_expr 0 env e)]
 | Zstreampat (x0,x1) -> chg_local_ident 0 (VAR env) (x0,x1)

 in chg_e ar ex
;;
 
