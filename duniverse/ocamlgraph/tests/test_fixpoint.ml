
(* Test file for Fixpoint *)

let id x = x
module IntOrdered = struct
  type t = int
  let compare: int -> int -> int = compare
  let hash: int -> int = id
  let equal: int -> int -> bool = (=)
end
module IntSet = Set.Make(IntOrdered)
module G = Graph.Persistent.Digraph.Concrete(IntOrdered)
module Divisors = Graph.Classic.P(G)
module Analysis = struct
  type data = IntSet.t
  type edge = G.edge
  type vertex = G.vertex
  type g = G.t
  let direction = Graph.Fixpoint.Backward
  let join = IntSet.union
  let equal = IntSet.equal
  let analyze (_: edge): data -> data = id
end
module Fixpoint = Graph.Fixpoint.Make(G)(Analysis)

let pp_int_set pp set =
  let first = ref true in
  Format.fprintf pp "@[<hov>";
  IntSet.iter (fun x ->
      if !first then
	first := false
      else 
	Format.fprintf pp ",@ ";
      Format.pp_print_int pp x)
    set;
  Format.fprintf pp "@]"

let () =
  let n = 15 in
  let labels = Fixpoint.analyze IntSet.singleton (Divisors.divisors n) in
  Format.open_vbox 0;
  for i = 2 to n do
    Format.printf "Labels for %d: %a@," i pp_int_set (labels i)
  done;
  Format.close_box ();
  Format.print_flush ()

