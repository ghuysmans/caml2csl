/* The parser definition */

%{
#open "globals";;
#open "syntax";;
#open "location";;
#open "lexer";;
#open "par_aux";;
%}

/* Tokens */

/* Identifiers, prefixes, infixes */
%token <string*location__location> IDENT
%token <string*location__location> PREFIX
%token <string*location__location> INFIX1
%token <string*location__location> INFIX2
%token <string*location__location> SUBTRACTIVE
%token <string*location__location> INFIX3
%token <string*location__location> INFIX4
/* Literals */
%token <int*location__location> INT
%token <char*location__location> CHAR
%token <float> FLOAT
%token <string*location__location> STRING
/* The end-of-file marker */
%token EOF
/* Special symbols */
%token <location__location>EQUAL          /* "=" */
%token <location__location>EQUALEQUAL     /* "==" */
%token SHARP          /* "#" */
%token AMPERSAND      /* "&" */
%token QUOTE          /* "'" */
%token LPAREN         /* "(" */
%token RPAREN         /* ")" */
%token <location__location>STAR           /* "*" */
%token COMMA          /* "," */
%token MINUSGREATER   /* "->" */
%token DOT            /* "." */
%token DOTDOT         /* ".." */
%token DOTLPAREN      /* ".(" */
%token DOTLBRACKET    /* ".[" */
%token COLON          /* ":" */
%token <location__location>COLONCOLON     /* "::" */
%token <location__location>COLONEQUAL     /* ":=" */
%token <location__location>SEMI           /* ";" */
%token SEMISEMI       /* ";;" */
%token LBRACKET       /* "[" */
%token LBRACKETBAR    /* "[|" */
%token LBRACKETLESS   /* "[<" */
%token LESSMINUS      /* "<-" */
%token RBRACKET       /* "]" */
%token <location__location>UNDERSCORE     /* "_" */
%token <location__location>UNDERUNDER     /* "__" */
%token LBRACE         /* "{" */
%token BAR            /* "|" */
%token BARRBRACKET    /* "|]" */
%token GREATERRBRACKET/* ">]" */
%token RBRACE         /* "}" */
%token AMPERAMPER     /* "&&" */
%token BARBAR         /* "||" */
/* Keywords */
%token <location__location>AND            /* "and" */
%token AS             /* "as" */
%token BEGIN          /* "begin" */
%token DO             /* "do" */
%token DONE           /* "done" */
%token DOWNTO         /* "downto" */
%token ELSE           /* "else" */
%token END            /* "end" */
%token <location__location>EXCEPTION      /* "exception" */
%token FOR            /* "for" */
%token <location__location>FUN            /* "fun" */
%token <location__location>FUNCTION       /* "function" */
%token IF             /* "if" */
%token IN             /* "in" */
%token LET            /* "let" */
%token MATCH          /* "match" */
%token MUTABLE        /* "mutable" */
%token <location__location>NOT            /* "not" */
%token OF             /* "of" */
%token OR             /* "or" */
%token <location__location>PREF           /* "prefix" */
%token <location__location>REC            /* "rec" */
%token THEN           /* "then" */
%token TO             /* "to" */
%token TRY            /* "try" */
%token TYPE           /* "type" */
%token <location__location>VALUE          /* "value" */
%token WHEN           /* "when" */
%token <location__location>WHERE          /* "where" */
%token WHILE          /* "while" */
%token <location__location>WITH           /* "with" */

/* Precedences and associativities. Lower precedences first. */

%right prec_let
%right prec_define
%right MINUSGREATER
%right WHERE
%right AND
%right SEMI
%right prec_list
%right prec_if
%right COLONEQUAL LESSMINUS
%left  AS
%left  BAR
%left  COMMA
%left  OR BARBAR
%left  AMPERSAND AMPERAMPER
%left  NOT
%left  INFIX1 EQUAL EQUALEQUAL          /* comparisons, concatenations */
%right COLONCOLON                       /* cons */
%left  INFIX2 SUBTRACTIVE               /* additives, subtractives */
%left  STAR INFIX3                      /* multiplicatives */
%right INFIX4                           /* exponentiations */
%right prec_uminus
%right prec_app
%left  DOT DOTLPAREN DOTLBRACKET
%right PREFIX                           /* prefix operators, e.g. ! */

/* Entry points */

%start Implementation
%type <syntax__impl_phrase> Implementation 
%start Interface
%type <syntax__intf_phrase> Interface

%%

/* One phrase from a module implementation */

Implementation :
        Expr SEMISEMI
          { make_impl(Zexpr $1) }
      | LET Binding_list SEMISEMI  %prec prec_let
          { make_impl(Zletdef(false, $2)) }
      | LET REC Binding_list SEMISEMI  %prec prec_let
          { make_impl(Zletdef(true, $3)) }
      | TYPE Type_decl SEMISEMI
          { make_impl(Ztypedef $2) }
      | EXCEPTION Exc_decl SEMISEMI
          { let (decl1,decls)=$2 in make_impl(Zexcdef (($1,decl1)::decls)) }
      | SHARP Directive SEMISEMI
          { make_impl(Zimpldirective $2) }
      | EOF
          { raise End_of_file }
;

/* One phrase from a module interface */

Interface :
        VALUE Value_decl SEMISEMI
          { let (decl1,decls)=$2 in make_intf(Zvaluedecl (($1,decl1)::decls)) }
      | TYPE Type_decl SEMISEMI
          { make_intf(Ztypedecl $2)}
      | EXCEPTION Exc_decl SEMISEMI
          { let (decl1,decls)=$2 in make_intf(Zexcdecl (($1,decl1)::decls)) }
      | SHARP Directive SEMISEMI
          { make_intf(Zintfdirective $2) }
      | EOF
          { raise End_of_file }
;

/* Expressions */

Expr :
        Simple_expr
          { $1 }
      | Simple_expr Simple_expr_list   %prec prec_app
          { make_apply NO_CHANGE ($1, $2) }
      | Expr_comma_list
          { make_expr(Ztuple(rev $1)) }
      | SUBTRACTIVE Expr  %prec prec_uminus
          { make_unop $1 $2 NO_CHANGE}
      | NOT Expr
          { let ({ e_loc= Loc (d1,f1)} as e2)= $2
            and (Loc (d2,f2))= get_current_location() in
                 make_unop ("not",$1) e2 
                 (SEQ [REPLACE ((Loc (d2,d2)),"(");
                       REPLACE ((Loc (d1,d1)),"(");
                       REPLACE ((Loc (f1,f1)),"))") ]) }
      | Ide LESSMINUS Expr
          { make_expr (Zassign($1, $3)) }
      | Expr INFIX4 Expr
          { make_infix $2 $1 $3 NO_CHANGE }
      | Expr INFIX3 Expr
          { make_infix $2 $1 $3 NO_CHANGE }
      | Expr INFIX2 Expr
          { let ((i,(Loc (d2,f2))) as iloc)=$2 and e1=$1 and e3=$3 in
            let (Loc (d1,_))=e1.e_loc 
            and (Loc (d3,f3))=e3.e_loc in
            if mem i !infix_list then
              make_binop iloc e1 e3
 (SWAP (e1.e_loc,(Loc (d2,f2)),[
   REPLACE ((Loc (d1,d1)),"(");
   REPLACE ((Loc (d2,d2)),"(");
   SEQ [ REPLACE ((Loc (f2,f2)),")"); REPLACE ((Loc (d3,d3)),"(") ];
   REPLACE ((Loc (f3,f3)),"))") ]))
            else
              make_infix iloc e1 e3 NO_CHANGE }
      | Expr SUBTRACTIVE Expr
          { make_infix $2 $1 $3 NO_CHANGE }
      | Expr INFIX1 Expr
          { make_infix $2 $1 $3 NO_CHANGE }
      | Expr STAR Expr
          { make_infix ("*",$2) $1 $3 NO_CHANGE }
      | Expr COLONCOLON Expr
          { make_expr(Zconstruct1((GIname (("::",$2),None)),
                        make_expr(Ztuple [$1; $3]) )) }
      | Expr EQUAL Expr
          { make_infix ("=",$2) $1 $3 NO_CHANGE }
      | Expr EQUALEQUAL Expr
          { make_infix ("==",$2) $1 $3 NO_CHANGE }
      | Expr AMPERSAND Expr
          { make_expr(Zsequand($1, $3)) }
      | Expr AMPERAMPER Expr
          { make_expr(Zsequand($1, $3)) }
      | Expr OR Expr
          { make_expr(Zsequor($1, $3)) }
      | Expr BARBAR Expr
          { make_expr(Zsequor($1, $3)) }
      | Simple_expr DOT Ext_ident LESSMINUS Expr
          { make_expr(Zrecord_update($1, $3, $5)) }
      | Simple_expr DOTLPAREN Expr RPAREN LESSMINUS Expr
          { make_ternop (".()<-",no_location) $1 $3 $6 NO_CHANGE }
      | Simple_expr DOTLBRACKET Expr RBRACKET LESSMINUS Expr
          { make_ternop (".[]<-",no_location) $1 $3 $6 NO_CHANGE }
      | Expr COLONEQUAL Expr
          { make_infix (":=",$2) $1 $3 NO_CHANGE }
      | IF Expr THEN Expr ELSE Expr  %prec prec_if
          { make_expr(Zcondition($2, $4, $6)) }
      | IF Expr THEN Expr  %prec prec_if
          { make_expr(Zcondition($2, $4,
                     make_expr(Zconstruct0 (def_gi "()")))) }
      | WHILE Expr DO Expr Opt_semi DONE
          { make_expr(Zwhile($2, $4)) }
      | FOR Ide EQUAL Expr TO Expr DO Expr Opt_semi DONE
          { make_expr_chg (Zfor($2, $4, $6, true, $8)) $9 }
      | FOR Ide EQUAL Expr DOWNTO Expr DO Expr Opt_semi DONE
          { make_expr_chg (Zfor($2, $4, $6, false, $8)) $9 }
      | Expr SEMI Expr
          { make_expr(Zsequence($1,$3)) }
      | Expr SEMI Expr SEMI
          { make_expr_chg (Zsequence($1,$3))
                 (SEQ [NO_CHANGE;NO_CHANGE;REPLACE ($4,"")]) }
      | MATCH Expr WITH Opt_bar Function_match
          { make_expr(Zapply($2,[make_expr(Zfunction (None,$5))])) }
      | MATCH Expr WITH Opt_bar Parser_match
          { let (pm,pmod)=$5 and (Loc (_,fin))= $3 in
            make_expr_chg(Zapply($2,[(make_expr_chg(Zparser pm)
                                   (SEQ (NO_CHANGE::pmod)))]))
                (SEQ [NO_CHANGE;
                      (REPLACE ((Loc (fin,fin))," parser"));
                      NO_CHANGE]) }
      | LET Binding_list IN Expr  %prec prec_let
          { make_expr(Zlet(false, $2, $4)) }
      | LET REC Binding_list IN Expr  %prec prec_let
          { make_expr(Zlet(true, $3, $5)) }
      | FUN Opt_bar Fun_match
          { make_expr(Zfunction ((Some $1),$3)) }
      | FUNCTION Opt_bar Function_match
          { make_expr(Zfunction (None,$3)) }
      | FUNCTION Opt_bar Parser_match
          { let (pm,pmod)=$3 in
              make_expr_chg(Zparser pm) (SEQ (REPLACE ($1,"parser")::pmod)) }
      | TRY Expr WITH Opt_bar Try_match
	  { make_expr(Ztrywith($2, $5)) }
      | Expr WHERE Binding_list
          {  let (Loc (deb1,fin2))=get_current_location() 
             and (Loc (fin1,deb2))=$2 in
              make_expr_chg (Zlet(false, $3, $1))
(SWAP (Loc (deb1,fin1), Loc (deb2,fin2),[
       REPLACE ((Loc (deb1,deb1)),"\n let");
       REPLACE ((Loc (fin1,deb2)),"\n in ");
       NO_CHANGE ])) }
      | Expr WHERE REC Binding_list  %prec WHERE
          {  let (Loc (deb1,fin2))=get_current_location() 
             and (Loc (fin1,deb2))=$2 in
              make_expr_chg (Zlet(true, $4, $1))
(SWAP (Loc (deb1,fin1), Loc (deb2,fin2),[
       REPLACE ((Loc (deb1,deb1)),"\n let");
       REPLACE ((Loc (fin1,deb2)),"\n in ");
       NO_CHANGE ])) }
;

Simple_expr :
        Atomic_constant
          { make_expr (Zconstant $1) }
      | Ext_ident
          { expr_constr_or_ident $1 }
      | LPAREN RPAREN
          { make_expr (Zconstruct0 (def_gi "()")) }
      | LBRACKET Expr_sm_list Opt_semi RBRACKET
          { make_list $2 $3 }
      | LBRACKET RBRACKET
          { make_expr (Zconstruct0 (def_gi "[]")) }
      | LBRACKETBAR Expr_sm_list Opt_semi BARRBRACKET
          { make_expr_chg (Zvector(rev $2)) (SEQ [ NO_CHANGE; $3 ]) }
      | LBRACKETBAR BARRBRACKET
          { make_expr(Zvector []) }
      | LBRACKETLESS Stream_expr Opt_semi GREATERRBRACKET
          { make_expr_chg (Zstream (rev $2)) $3 }
      | LBRACKETLESS GREATERRBRACKET
          { make_expr(Zstream []) }
      | LPAREN Expr COLON Type RPAREN
          { make_expr(Zconstraint($2, $4)) }
      | LPAREN Expr RPAREN
          { let {e_desc= desc; e_chg= chg }=$2 in make_expr_chg desc chg }
      | BEGIN Expr Opt_semi END
          { let {e_desc= desc; e_chg= chg }=$2 in make_expr_chg desc chg }
      | LBRACE Expr_label_list Opt_semi RBRACE
          { make_expr_chg (Zrecord $2) $3 }
      | PREFIX Simple_expr
          { make_unop $1 $2 NO_CHANGE}
      | Simple_expr DOT Ext_ident
          { make_expr(Zrecord_access($1, $3)) }
      | Simple_expr DOTLPAREN Expr RPAREN
          { make_binop (".(",no_location) $1 $3 NO_CHANGE }
      | Simple_expr DOTLBRACKET Expr RBRACKET
          { make_binop (".[",no_location) $1 $3 NO_CHANGE }
;

Simple_expr_list :
        Simple_expr Simple_expr_list
          { $1 :: $2 }
      | Simple_expr
          { [$1] }
;

Expr_comma_list :
        Expr_comma_list COMMA Expr
          { $3 :: $1 }
      | Expr COMMA Expr
          { [$3; $1] }
;

Expr_sm_list :
        Expr_sm_list SEMI Expr  %prec prec_list
          { $3 :: $1 }
      | Expr  %prec prec_list
          { [$1] }
;

Opt_semi :
        SEMI            { REPLACE ($1,"") }
      | /*epsilon*/     { NO_CHANGE }
;

Expr_label :
        Ext_ident EQUAL Expr
          { ($1, $3)  }
;

Expr_label_list :
        Expr_label_list SEMI Expr_label  %prec prec_list
          { $3 :: $1 }
      | Expr_label  %prec prec_list
          { [$1] }
;

/* Constants */

Atomic_constant :
        INT
          { ACother }
      | FLOAT
          { ACother }
      | STRING
          { ACother }
      | CHAR
          { ACchar $1 }
;

/* Definitions by pattern matchings */

Opt_bar:
        BAR             { () }
      | /*epsilon*/     { () }
;

Action :
        MINUSGREATER Expr
          { $2 }
      | WHEN Expr MINUSGREATER Expr
          { make_expr (Zwhen($2,$4)) }
;

Fun_match :
        Simple_pattern_list Action BAR Fun_match
          { ($1, $2) :: $4}
      | Simple_pattern_list Action
	  { [$1, $2] }
;

Function_match :
        Pattern Action BAR Function_match
          { ([$1], $2) :: $4 }
      | Pattern Action
	  { [[$1], $2] }
;

Try_match :
        Pattern Action BAR Try_match
          { ($1, $2) :: $4 }
      | Pattern Action
          { [$1, $2] }
;

Binding_list :
        Binding AND Binding_list
          { $1 :: $3 }
      | Binding
          { [$1] }
;

Binding :
        Pattern EQUAL Expr  %prec prec_define
          { ($1, $3) }
      | Ide Simple_pattern_list EQUAL Expr  %prec prec_define
          { (make_pat(Zvarpat $1),
                make_expr(Zfunction (None,[$2, $4]))) }
;

/* Patterns */

Pattern_sm_list :
        Pattern SEMI Pattern_sm_list
          { make_pat(Zconstruct1pat ((def_gi "::"),
                      make_pat(Ztuplepat[$1; $3]))) }
      | Pattern Opt_semi
          { make_pat(Zconstruct1pat((def_gi "::"),
              make_pat(Ztuplepat [$1;
               make_pat_chg (Zconstruct0pat (def_gi "[]"))
                                 (SEQ [ NO_CHANGE; $2 ])]))) }
;

Pattern_label_list :
        Pattern_label SEMI Pattern_label_list
          { let (p3,chg)=$3 in
             if p3=[] then [$1], [NO_CHANGE; SEQ ((REPLACE ($2,""))::chg) ]
             else ($1 :: p3),(NO_CHANGE::chg) }
      | Pattern_label Opt_semi
          { [$1],[NO_CHANGE;$2] }
      | UNDERSCORE Opt_semi
          { [],[ SEQ [(REPLACE ($1,""));$2] ] }
;

Pattern_label :
        Ext_ident EQUAL Pattern
          { ( $1, $3) }
;

Pattern_comma_list :
        Pattern_comma_list COMMA Pattern
          { $3 :: $1 }
      | Pattern COMMA Pattern
          { [$3; $1] }
;
  
Simple_pattern_list :
        Simple_pattern Simple_pattern_list
          { $1 :: $2 }
      | Simple_pattern
          { [$1] }
;

Pattern :
        Simple_pattern
          { $1 }
      | Pattern AS IDENT
          { make_pat(Zaliaspat($1, $3)) }
      | Pattern COLONCOLON Pattern
          { make_pat(Zconstruct1pat((GIname (("::",$2),None)),
              make_pat(Ztuplepat [$1; $3]))) }
      | Pattern_comma_list
          { make_pat(Ztuplepat(rev $1)) }
      | Ext_ident Simple_pattern
          { let x1=$1 and x2=$2 in
              make_pat_chg (Zconstruct1pat ( x1, x2))
                 (special_pat_constr1 (get_current_location()) x1) }
      | Pattern BAR Pattern
          { make_pat(Zorpat($1, $3)) }
;

Simple_pattern :
        Atomic_constant
          { make_pat(Zconstantpat $1) }
      | SUBTRACTIVE INT
          { make_pat(Zconstantpat(ACother)) }
      | SUBTRACTIVE FLOAT
          { make_pat(Zconstantpat(ACother)) }
      | UNDERSCORE
          { make_pat(Zwildpat) }
      | Ide
          { pat_constr_or_var $1 }
      | Qual_ident
          { let x=$1 in
              expr_constr_or_ident x; make_pat(Zconstruct0pat x) }
      | LPAREN RPAREN
          { make_pat (Zconstruct0pat (def_gi "()")) }
      | LBRACKET RBRACKET
          { make_pat (Zconstruct0pat (def_gi "[]")) }
      | LBRACKET Pattern_sm_list RBRACKET
          { let p2=$2 in make_pat_chg p2.p_desc p2.p_chg }
      | LPAREN Pattern COLON Type RPAREN
          { make_pat(Zconstraintpat($2, $4)) }
      | LBRACE Pattern_label_list RBRACE
          { let (p2,chg)=$2 in make_pat_chg (Zrecordpat p2) (SEQ chg) }
      | LPAREN Pattern RPAREN
          { let p2=$2 in make_pat_chg p2.p_desc p2.p_chg }
      | CHAR DOTDOT CHAR
          { make_pat (Zorpat (
                       (make_pat(Zconstantpat(ACchar $1))),
                        (make_pat(Zconstantpat(ACchar $3))))) }
;

/* Streams */

Stream_expr :
        Stream_expr SEMI Stream_expr_component  %prec prec_list
          { $3 :: $1 }
      | Stream_expr_component  %prec prec_list
          { [$1] }
;

Stream_expr_component :
        Expr %prec prec_list
          { Znonterm $1 }
      | QUOTE Expr  %prec prec_list
          { Zterm $2 }
;

Stream_pattern :
        LBRACKETLESS GREATERRBRACKET
          { ([], NO_CHANGE ) }
      | LBRACKETLESS Stream_pattern_component_list Opt_semi GREATERRBRACKET
          { ($2, $3) }
;

Stream_pattern_component_list :
        Stream_pattern_component
          { [$1] }
      | IDENT
          { let (s,loc)=$1 in [Zstreampat (s,loc)] }
      | Stream_pattern_component SEMI Stream_pattern_component_list
          { $1 :: $3 }
;

Stream_pattern_component :
        Simple_expr Simple_pattern
          { Znontermpat($1, $2) }
      | QUOTE Pattern
          { Ztermpat $2 }
;

Parser_match :
        Stream_pattern MINUSGREATER Expr BAR Parser_match
          { let (lp,md)= $1 and (pm,lmd)=$5 in ((lp, $3) :: pm),(md :: lmd) }
      | Stream_pattern MINUSGREATER Expr
	  { let (lp,md)= $1 in [(lp, $3)], [md] }
;

/* Identifiers */

Ide :
        IDENT
          { ($1,None) }
      | PREF Infx
          { ($2,(Some $1)) }
;

Infx :
        INFIX1        { $1 }
      | INFIX2        { $1 }
      | INFIX3        { $1 }
      | INFIX4        { $1 }
      | STAR          { "*",$1 }   
      | COLONCOLON    { "::",$1 }
      | COLONEQUAL    { ":=",$1 }
      | EQUAL         { "=",$1 }
      | EQUALEQUAL    { "==",$1 }
      | NOT           { "not",$1 }
      | SUBTRACTIVE   { $1 }
      | PREFIX        { $1 }
;
  
Qual_ident :
        IDENT UNDERUNDER Ide
          { let (mn,ml)=$1 and ((vn,vl),vpo)=$3 in
            (GImodname ({qual=mn; id=vn},ml,$2,vpo,vl)) }
;

Ext_ident :
        Qual_ident
          { $1 }
      | Ide
          { let (sloc,prf)=$1 in GIname (sloc,prf) }
;

/* Type expressions */

Type :
        Simple_type
          { $1 }
      | Type_star_list
          { make_typ(Ztypetuple(rev $1)) }
      | Type MINUSGREATER Type
          { make_typ(Ztypearrow($1, $3)) }
;

Simple_type :
        Type_var
          { make_typ(Ztypevar $1) }
      | Ext_ident
          { make_typ(Ztypeconstr($1, [])) }
      | Simple_type Ext_ident
          { make_typ(Ztypeconstr($2, [$1])) }
      | LPAREN Type COMMA Type_comma_list RPAREN Ext_ident
          { make_typ(Ztypeconstr($6, $2 :: $4)) }
      | LPAREN Type RPAREN
          { make_typ $2.te_desc }
;

Type_star_list :
        Type_star_list STAR Simple_type
          { $3 :: $1 }
      | Simple_type STAR Simple_type
          { [$3; $1] }
;

Type_var :
        QUOTE IDENT
          { $2 }
;

Type_comma_list :
        Type COMMA Type_comma_list
          { $1 :: $3 }
      | Type
          { [$1] }
;

/* Declarations */

Value_decl :
        Value1_decl AND Value_decl
          { let (d1,ds)=$3 in ($1,(($2,d1)::ds)) }
      | Value1_decl
          { ($1,[]) }
;

Type_decl :
        Type1_decl AND Type_decl
          { $1 :: $3 }
      | Type1_decl
          { [$1] }
;

Exc_decl :
        Constr1_decl AND Exc_decl
          { let (d1,ds)=$3 in ($1,(($2,d1)::ds)) }
      | Constr1_decl
          { $1,[] }
;

Constr_decl :
        Constr1_decl BAR Constr_decl
          { $1 :: $3 }
      | Constr1_decl
          { [$1] }
;

Label_decl :
        Label1_decl SEMI Label_decl
          { $1 :: $3 }
      | Label1_decl Opt_semi
          { [$1] }
;

Value1_decl :
        Ide COLON Type
          { ($1, $3, None) }
      | Ide COLON Type EQUAL Prim_decl
          { ($1, $3, (Some $5)) }
;

Prim_decl :
        INT STRING
          { (snd $1) }
;

Type1_decl :
        Type_params IDENT Type1_def
          { ($2, $1, $3) }
;

Type1_def :
        /* epsilon */
          { Zabstract_type }
      | EQUAL Opt_bar Constr_decl
          { Zvariant_type $3 }
      | EQUAL LBRACE Label_decl RBRACE
          { Zrecord_type $3 }
      | EQUALEQUAL Type
          { Zabbrev_type ($1,$2) }
;

Constr1_decl :
        IDENT OF Mutable_option Type
          { Zconstr1decl($1, $4, $3) }
      | IDENT
          { Zconstr0decl $1 }
;

Label1_decl :
        Mutable_option IDENT COLON Type
          { ($2, $4, $1) }
;

Mutable_option :
        MUTABLE
          { Mutable }
      | /* epsilon */
          { Notmutable }
;

Type_params :
        LPAREN Type_var_list RPAREN
          { $2 }
      | Type_var
          { [$1] }
      |
          { [] }
;

Type_var_list :
        Type_var COMMA Type_var_list
          { $1 :: $3 }
      | Type_var
          { [$1] }
;
       
/* Directives */

Directive :
        IDENT STRING
          { Zdir((fst $1),$2) }
;

%%
