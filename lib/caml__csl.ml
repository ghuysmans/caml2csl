
(* arg *)

let arg_parse opt_list anonymous_fun = 
  Arg.parse (List.map (fun (ext,act) -> (ext,act,"---")) opt_list)
       anonymous_fun "---"
;;


(* io *)

external open_descriptor_out : int -> out_channel = "open_descriptor"
external open_descriptor_in : int -> in_channel = "open_descriptor"



(* list *)

let flat_map f =
 let rec flat_map_f = function
     [] -> [] | x::l -> f x @ flat_map_f l
 in flat_map_f
;;

let except e =
 let rec except_e = function
     [] -> []
   | elem::l -> if e = elem then l else elem::except_e l
 in except_e
;;

let exceptq e =
 let rec exceptq_e = function
     [] -> []
   | elem::l -> if e == elem then l else elem::exceptq_e l
 in exceptq_e
;;

let subtract f= function
    [] -> f
  | e  ->
     let rec subtract_e = function
      [] -> []
    | elem::l -> if List.mem elem e then subtract_e l else elem :: subtract_e l
     in subtract_e f
;;

let union l1 l2 =
  let rec union_rec = function
    [] -> l2
  | a::l -> if List.mem a l2 then union_rec l else a :: union_rec l
  in union_rec l1
;;

let intersect l1 l2 =
 let rec inter_rec = function
    [] -> []
  | a::l -> if List.mem a l2 then a :: inter_rec l else inter_rec l
 in inter_rec l1
;;

let index a =
 let rec index_rec i = function
     []  -> raise Not_found
  | b::l -> if a = b then i else index_rec (succ i) l
 in index_rec 0
;;





(* pair *)

let rec combine (l1, l2)=  List.combine l1 l2;;

let map_combine f =
  let rec map = function
    [], [] -> []
  | h1::t1, h2::t2 -> f (h1,h2) :: map (t1,t2)
  | _ -> invalid_arg "map_combine"
  in map
;;

let do_list_combine f =
  let rec dol = function
    [], [] -> ()
  | h1::t1, h2::t2 -> f (h1,h2); dol (t1,t2)
  | _ -> invalid_arg "do_list_combine"
  in dol
;;





(* stream *)

let rec from f=
  try [< 'f() ; (from f) >]
  with Stream.Parse_failure -> [< >]
;;

(* Stream.of_string and Stream.of_channel are not equivalent to those funs *)
let stream_of_string s =
  let cnt = ref (-1) in
  from (fun () -> incr cnt;
                  if !cnt > String.length s 
                  then raise Stream.Parse_failure
                  else s.[ !cnt ])
;;

let stream_of_channel ch =
  from (fun () -> try input_char ch
                  with End_of_file -> raise Stream.Parse_failure)
;;

let check p= parser [< 'x when p x >] -> x;;





(* string *)

let replace_string dest src pos =
  if pos < 0 or pos + String.length src > String.length dest
  then invalid_arg "replace_string"
  else String.blit src 0 dest pos (String.length src)
;;

let compare_strings s1 s2=
  let l1= String.length s1
  and l2= String.length s2 in
  let rec cs n=
    if n >= l1 then
      if n >= l2 then 0
      else 2
    else if n >= l2 then -2
    else if (String.get s1 n) = (String.get s2 n) then cs (succ n)
    else if (String.get s1 n) < (String.get s2 n) then -1
    else 1
  in cs 0
;;



(* vect *)

let map_vect_list f v=
  let rec mvlrec l=function
     0 -> l
   | n -> let k=pred n in mvlrec ((f v.(k))::l) k
  in mvlrec [] (Array.length v)
;;



(* format *)

let print_break (nspaces, offset)= Format.print_break nspaces offset;;


(* hashtbl *)

let do_table_rev f h =
  failwith "hashtbl__do_table_rev: no more available"
;;


(* sys *)

type file_perm = int;;
let s_irusr= 256;;
let s_iwusr= 128;;
let s_ixusr= 64;;
let s_irgrp= 32;;
let s_iwgrp= 16;;
let s_ixgrp= 8;;
let s_iroth= 4;;
let s_iwoth= 2;;
let s_ixoth= 1;;
let s_isuid= 2048;;
let s_isgid= 1024;;
let s_irall= 292;;
let s_iwall= 146;;
let s_ixall= 73;;

let interactive= !Sys.interactive;;

let sys_open f ofl fperm=
  failwith "sys__open not converted"
;;

let sys_close fdesc=
  failwith "sys__close not converted"
;;



