
#open "print";;
#open "globals";;
#open "location";;
#open "syntax";;

(* effectuer les changements *)

let i_f=ref stdin;;
let o_f=ref stdout;;

let input_n n=
  let s=create_string n in
  really_input !i_f s 0 n;
  s
;;

let output_str s n=
  output !o_f s 0 n
;;

let copy_til n=
  let nb = n - (pos_in !i_f) in
  inform ("copy ->" ^ (string_of_int n));
  if nb < 0 then failwith
            ("Chevauchement: "^(string_of_int n)^"->"^(string_of_int (-nb)))
   else output_str (input_n nb) nb
;;

let copy_til_eof()=
  let nb=(in_channel_length !i_f) - (pos_in !i_f) in
  let s=create_string nb in
    really_input !i_f s 0 nb;
    output_str s nb
;;

let emit_chg (Loc (deb,fin)) str =
  inform ((string_of_loc (Loc (deb,fin))) ^ ": chg " ^ str);
  copy_til deb;
  output_str str (string_length str);
  seek_in !i_f fin
;;

let emit_skip_from_to f t =
  inform ("skip "^(string_of_loc (Loc (f,t))));
  copy_til f;
  seek_in !i_f t
;;


let begin_chg filename1 filename2=
  i_f := open_in filename1;
  o_f := open_out filename2
;;

let end_chg()=
  copy_til_eof();
  close_in !i_f;
  flush !o_f;
  close_out !o_f
;;





let rec do_chg= function
  NO_CHANGE -> ()
| SEQ l -> do_list do_chg l
| REPLACE (loc,str) -> emit_chg loc str
| SWAP (l1,l2,chg) -> simple_swap l1 l2 chg

and simple_swap (Loc (deb1,fin1)) (Loc (deb2,fin2))= fun
  [x1;x2] -> do_chg x1;
             emit_skip_from_to deb1 deb2;
             emit_skip_from_to fin2 fin1;
             do_chg x2;
             emit_skip_from_to deb2 deb1;
             emit_skip_from_to fin1 fin2
| _ -> failwith "simple SWAP anomaly"
;;


let rec do_synchro f l= function
  SEQ (bef::seq) -> do_chg bef; do_list2 f seq l
| SWAP (l1,l2,chg) -> do_swap l1 l2 f chg l
| x -> do_chg x; do_list (f NO_CHANGE) l

and do_swap (Loc (deb1,fin1)) (Loc (deb2,fin2)) f= fun
  (bef::bet::aft) (x1::x2::xaft)
    ->  inform ((string_of_loc (Loc (deb1,fin1))) 
              ^ "<->" ^ (string_of_loc (Loc (deb2,fin2))));
        do_chg bef;
        emit_skip_from_to deb1 deb2;
        f NO_CHANGE x1;
        emit_skip_from_to fin2 fin1;
        do_chg bet;
        emit_skip_from_to deb2 deb1;
        f NO_CHANGE x2;
        emit_skip_from_to fin1 fin2;
        do_synchro f xaft (SEQ aft)
| _ _ -> failwith "SWAP anomaly"
;;

let do_default f l seq= do_synchro (fun chg x -> f x; do_chg chg) l seq;;

let do_frozen= do_default (fun (f: unit->unit) -> f());;
