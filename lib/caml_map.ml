(* Association tables over ordered types *)


open Baltree;;

type ('a, 'b) binding =
  { key: 'a; data: 'b; prev: 'b list };;

type ('a, 'b) t =
  { tree: ('a, 'b) binding Baltree.t;
    order: 'a -> ('a, 'b) binding -> int };;

let empty ord =
  { tree = Empty; order = fun x y -> ord x y.key };;

let bind x y b =
  Something
    { key = x;
      data = y;
      prev = match b with Nothing -> [] | Something b -> b.data :: b.prev };;

let add x y m =
  { tree = Baltree.modify (m.order x) (bind x y) m.tree;
    order = m.order };;

let find x m =
  (Baltree.find (m.order x) m.tree).data;;

let unbind = function
    Something({prev = x::l} as b) ->
      Something { key = b.key; data = x; prev = l }
  | _ -> Nothing;;

let remove x m =
  { tree = Baltree.modify (m.order x) unbind m.tree;
    order = m.order };;

let iter f m =
  let rec iter = function
    Empty -> ()
  | Node(l, b, r, _) ->
      iter l;
      ignore (f b.key b.data);
      List.iter (fun x -> ignore (f b.key x)) b.prev;
      iter r
  in iter m.tree;;
