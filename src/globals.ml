(* Various useful stuff *)


let verbose = ref false;;
let warn_flag = ref true;;
let core_lib = ref "";;
let output_lib = ref "";;
let load_path = ref ([] : string list);;

let uses_conv= ref false;;


let default_used_interfaces =
  ["cautious", ["builtin"; "stream"; "exc"; "bool"; "string"; "char"; "vect";
                "list"; "pair"; "ref"; "float"; "int"; "eq"; "io"];
   "fast",     ["builtin"; "stream"; "exc"; "bool"; "fstring"; "fchar";
                "fvect"; "list"; "pair"; "ref"; "float"; "int"; "eq"; "io"];
   "none",     ["builtin"]]
;;



type 'a option=
   None
 | Some of 'a
;;


let inform s =
  if !verbose then (print_endline s; flush stdout)
;;

let warn s =
  if !warn_flag then prerr_endline ("Warning: " ^ s ^ ".")
;;

let todo s =
  prerr_endline ("-- TO DO -- : " ^ s ^ ".")
;;


let file_exists filename =
  try
    close_in (open_in filename); true
  with sys__Sys_error _ ->
    false
;;

exception Cannot_find_file of string;;

let find_in_path filename =
  if file_exists filename then
    filename
  else if filename__is_absolute filename then
    raise(Cannot_find_file filename)
  else
    let rec find = function
      [] ->
        raise(Cannot_find_file filename)
    | a::rest ->
        let b = filename__concat a filename in
          if file_exists b then b else find rest
    in find !load_path
;;

let remove_file f =
  try
    sys__remove f
  with sys__Sys_error _ ->
    ()
;;



let ext_id s n = s ^ (string_of_int n);;

let mk_list sep l=
  let rec mpl= function
    [s] -> s
  | p::(_::_ as t) -> (mpl t)^sep^p
  | _ -> failwith "globals__mk_list: empty list"  in
  mpl l
;;


let lower c=
  char_of_int (if c>=65 & c<=90 then c+32 else c)
;;

let upper c=
  char_of_int (if c>=97 & c<=122 then c-32 else c)
;;

let change_case f s=
  let s'=s^"" in
  if s'<>"" then s'.[0] <- f (int_of_char s'.[0]);
  s'
;;


let add_table t1 t2 =
  hashtbl__do_table_rev (hashtbl__add t2) t1
;;

let remove_from_table tbl x =
  hashtbl__do_table (fun y _ -> if x = y then hashtbl__remove tbl x) tbl
;;

let table_add tbl x y =
  remove_from_table tbl x;
  hashtbl__add tbl x y
;;
