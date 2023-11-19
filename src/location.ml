(* Printing a location in the source program *)

open Parsing;;

type location =
    Loc of int     (* Position of the first character *)
         * int     (* Position of the next character following the last one *)
;;

let no_location =
  Loc(0,0)
;;


(*
let union (Loc (d1,f1)) (Loc (d2,f2)) = Loc ((min d1 d2),(max f1 f2));;
*)

let get_current_location () =
  Loc(symbol_start(), symbol_end())
;;
