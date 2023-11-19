(* The abstract syntax for the language *)

open Location;;


type qualified_ident =
  { qual: string; id: string }
;;

type global_reference =
    GRname of string
  | GRmodname of qualified_ident
;;

type mutable_flag =
  Mutable | Notmutable
;;

(* location of an unqualified identifer, with optional prefix location *)
type local_ident = (string*location)*(location option);;

(* GImodname: qual_id, location of module, dot, optional prefix, ident *)
type global_ident=
  GIname of local_ident
| GImodname of qualified_ident * location * location
                 * (location option) * location
;;

let loc_of_gi = function
  GIname ((_,loc),_) -> loc
| GImodname (_,_,_,_,loc) -> loc
;;

type change=
     NO_CHANGE
   | SEQ of change list
   | REPLACE of location*string
   | SWAP of location*location*change list
;;

type atomic_constant =
    ACchar of char*location
  | ACother
;;

type type_expression =
  { te_desc: type_expression_desc;
    te_loc: location }
and type_expression_desc =
    Ztypevar of string*location
  | Ztypearrow of type_expression * type_expression
  | Ztypetuple of type_expression list
  | Ztypeconstr of global_ident * type_expression list
;;

type pattern =
  { p_desc: pattern_desc;
    p_loc: location;
    p_chg: change}
and pattern_desc =
    Zwildpat
  | Zvarpat of local_ident
  | Zaliaspat of pattern * (string*location)
  | Zconstantpat of atomic_constant
  | Ztuplepat of pattern list
  | Zconstruct0pat of global_ident
  | Zconstruct1pat of global_ident * pattern
  | Zorpat of pattern * pattern
  | Zconstraintpat of pattern * type_expression
  | Zrecordpat of (global_ident * pattern) list
;;

type expression =
  { e_desc: expression_desc;
    e_loc: location;
    e_chg: change }
and expression_desc =
    Zident of expr_ident ref
  | Zconstant of atomic_constant
  | Ztuple of expression list
  | Zconstruct0 of global_ident
  | Zconstruct1 of global_ident * expression
  | Zapply of expression * expression list
  | Zlet of bool * (pattern * expression) list * expression
  | Zfunction of location option * (pattern list * expression) list
  | Ztrywith of expression * (pattern * expression) list
  | Zsequence of expression * expression
  | Zcondition of expression * expression * expression
  | Zwhile of expression * expression
  | Zfor of local_ident * expression * expression * bool * expression
  | Zsequand of expression * expression
  | Zsequor of expression * expression
  | Zconstraint of expression * type_expression
  | Zvector of expression list
  | Zassign of local_ident * expression
  | Zrecord of (global_ident * expression) list
  | Zrecord_access of expression * global_ident
  | Zrecord_update of expression * global_ident * expression
  | Zstream of stream_component list
  | Zparser of (stream_pattern list * expression) list
  | Zwhen of expression * expression

and expr_ident =
    Zglobal of global_ident
  | Zlocal of local_ident

and stream_component =
    Zterm of expression
  | Znonterm of expression

and stream_pattern =
    Ztermpat of pattern
  | Znontermpat of expression * pattern
  | Zstreampat of string*location
;;

type type_decl =
    Zabstract_type
  | Zvariant_type of constr_decl list
  | Zrecord_type of ((string*location) * type_expression * mutable_flag) list
  | Zabbrev_type of location * type_expression

and constr_decl =
    Zconstr0decl of (string*location)
  | Zconstr1decl of (string*location) * type_expression * mutable_flag
;;

type directiveu =
    Zdir of string * (string*location)
;;

type impl_phrase =
  { im_desc: impl_desc;
    im_loc: location }
and impl_desc =
    Zexpr of expression
  | Zletdef of bool * (pattern * expression) list
  | Ztypedef of ((string*location)
                    * (string*location) list * type_decl) list
  | Zexcdef of (location * constr_decl) list
  | Zimpldirective of directiveu
;;

type intf_phrase =
  { in_desc: intf_desc;
    in_loc: location }
and intf_desc =
    Zvaluedecl of (location*
         (local_ident * type_expression * location option)) list
  | Ztypedecl of ((string*location)
                    * (string*location) list * type_decl) list
  | Zexcdecl of (location * constr_decl) list
  | Zintfdirective of directiveu
;;
