
(* arg *)

val arg_parse: (string * Arg.spec) list -> (string -> unit) -> unit


(* io *)

external open_descriptor_out : int -> out_channel = "open_descriptor"
external open_descriptor_in : int -> in_channel = "open_descriptor"



(* list *)

val flat_map : ('a -> 'b list) -> 'a list -> 'b list
val except : 'a -> 'a list -> 'a list
val exceptq : 'a -> 'a list -> 'a list
val subtract : 'a list -> 'a list -> 'a list
val union : 'a list -> 'a list -> 'a list
val intersect : 'a list -> 'a list -> 'a list
val index : 'a -> 'a list -> int


(* pair *)

val combine: 'a list * 'b list -> ('a * 'b) list
val map_combine : ('a * 'b -> 'c) -> 'a list * 'b list -> 'c list
val do_list_combine : ('a * 'b -> 'c) -> 'a list * 'b list -> unit





(* stream *)

val from : (unit -> 'a) -> 'a Stream.t
val check : ('a -> bool) -> 'a Stream.t -> 'a
val stream_of_string : string -> char Stream.t
val stream_of_channel: in_channel -> char Stream.t

(* string *)


val replace_string : string -> string -> int -> unit
val compare_strings : string -> string -> int

(* vect *)

val map_vect_list : ('a -> 'b) -> 'a array -> 'b list




(* format *)

val print_break: int * int -> unit


(* hashtbl *)

val do_table_rev: ('a -> 'b -> 'c) -> ('a, 'b) Hashtbl.t -> unit


(* sys *)

type file_perm = int;;
val s_irusr: int
val s_iwusr: int
val s_ixusr: int
val s_irgrp: int
val s_iwgrp: int
val s_ixgrp: int
val s_iroth: int
val s_iwoth: int
val s_ixoth: int
val s_isuid: int
val s_isgid: int
val s_irall: int
val s_iwall: int
val s_ixall: int
val interactive : bool

val sys_open : string -> open_flag list -> file_perm -> int
val sys_close : int -> unit


