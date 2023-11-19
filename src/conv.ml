
open Print;;
open Globals;;
open Syntax;;
open Modules;;


type new_name = ID of string
              | LIST_ID of string
;;

type table = (string*new_name) list;;

type genre = VAR of table | TYP | CONSTR of int | LABEL;;

let sel_of_genre = function
   VAR _ -> values_of_module
 | TYP -> types_of_module
 | CONSTR _ -> constrs_of_module
 | LABEL -> labels_of_module
;;

let case_of_genre = function
   VAR _ -> lower
 | TYP -> lower
 | CONSTR _ -> upper
 | LABEL -> lower
;;

(* local name conversions *)

let used_csl_unqual_id sel_fct s=
    try Hashtbl.find (sel_fct (find_csl_module "__")) s; true
    with Not_found ->
     (try Hashtbl.find (sel_fct (find_csl_module !csl_def_mod)) s;true
      with Not_found -> false)
;;

let used_csl_unqual_var (env:table) s=
  (List.mem (ID s) (snd (List.split env))) or (used_csl_unqual_id values_of_module s)
;;



let find_free_ext env s=
  let rec try_succ n=
    let name = ext_id s n in
    if (used_csl_unqual_var env name)
     then try_succ (succ n)
     else name  in
  try_succ 0
;;

let extend_var env x=
  let s= change_case lower x in
  if used_csl_unqual_var env s then find_free_ext env s else s
;;



let rec conv_local_var (env:table) x=
  try match (List.assoc x env) with
        ID s -> s   (* preservation des captures *)
      | sl -> (conv_local_var (Caml__csl.except (x,sl) env) x)
  with Not_found ->
    (try (Hashtbl.find !defined_module.mod_values x).id
     with Not_found ->
       (try (Hashtbl.find !conv_hints.mod_values x).id
        with Not_found ->
          (try (Hashtbl.find !opened_modules.mod_values x).id
           with Not_found -> extend_var env x)))
;;


let add_local_var (env:table) x=
  (x, ID(conv_local_var env x)) :: env
;; 


(* constructeurs d'arite superieure a 1 *)

let adapt_arity x env ar=
  let rec name_list ev l= function
    0 -> (x, LIST_ID("("^(mk_list "," l)^")")) :: ev
  | n -> let s=find_free_ext ev "x" in
          name_list (("", ID s) :: ev) (s::l) (pred n)
  in name_list env [] ar
;;



(* conversions in patterns *)

let rec conv_pat arity env pat =
  let check_arity what =
    if arity > 1 then todo ("Pattern in " ^ (string_of_loc pat.p_loc)
                     ^ " should be a " ^ (string_of_int arity)
                     ^ "-uple\n\t\tinstead of " ^ what) in 
  match pat.p_desc with
    Zwildpat -> env
  | Zvarpat ((v,_),_) -> if arity > 1 then adapt_arity v env arity
                          else add_local_var env v
  | Zaliaspat (pat,(v,_)) -> check_arity "an alias";
        let ev=conv_pat 0 env pat in add_local_var ev v
  | Zconstantpat _ -> env
  | Ztuplepat patl -> conv_pat_list 0 env patl
  | Zconstruct0pat _ -> env
  | Zconstruct1pat (gi, pat) -> conv_pat (arity_of gi) env pat
  | Zorpat (pat1, pat2) -> check_arity "an alternative";
                           conv_pat_list arity env [pat1;pat2]
  | Zconstraintpat (pat, _) -> check_arity "a type cast";
                               conv_pat arity env pat
  | Zrecordpat lbl_pat_list -> conv_pat_list 0 env (snd (List.split lbl_pat_list))

and conv_pat_list arity env pat_list= List.fold_left (conv_pat arity) env pat_list
;;  
  

(* conversions in stream patterns *)

let conv_strm_pat env = function 
   Ztermpat p -> conv_pat 0 env p
 | Znontermpat (_,p) -> conv_pat 0 env p
 | Zstreampat (s,_) -> add_local_var env s
;;

