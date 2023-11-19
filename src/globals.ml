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
  with Sys_error _ ->
    false
;;

exception Cannot_find_file of string;;

let find_in_path filename =
  if file_exists filename then
    filename
  else if not (Filename.is_relative filename) then
    raise(Cannot_find_file filename)
  else
    let rec find = function
      [] ->
        raise(Cannot_find_file filename)
    | a::rest ->
        let b = Filename.concat a filename in
          if file_exists b then b else find rest
    in find !load_path
;;

let remove_file f =
  try
    Sys.remove f
  with Sys_error _ ->
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
  Char.chr (if c>=65 & c<=90 then c+32 else c)
;;

let upper c=
  Char.chr (if c>=97 & c<=122 then c-32 else c)
;;

let change_case f s=
  let s'=s^"" in
  if s'<>"" then s'.[0] <- f (Char.code s'.[0]);
  s'
;;


let add_table t1 t2 =
  Caml__csl.do_table_rev (Hashtbl.add t2) t1
;;

let remove_from_table tbl x =
  Hashtbl.iter (fun y _ -> if x = y then Hashtbl.remove tbl x) tbl
;;

let table_add tbl x y =
  remove_from_table tbl x;
  Hashtbl.add tbl x y
;;
