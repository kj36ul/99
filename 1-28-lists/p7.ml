(*

Flatten a nested list structure. (medium)

# (* There is no nested list type in OCaml, so we need to define one
     first. A node of a nested list is either an element, or a list of
     nodes. *)
  type 'a node =
    | One of 'a 
    | Many of 'a node list;;
type 'a node = One of 'a | Many of 'a node list

*)


(* Define a recursive type 'a node that can either be a single element (One) or a list of nodes (Many) *)
type 'a node = One of 'a | Many of 'a node list

(* Function to flatten a nested list of 'a node into a single list of elements *)
let flatten nl =
  (* Helper recursive function to accumulate the flattened result *)
  let rec flat acc = function
    (* Case: empty list, return the accumulated result reversed *)
    | [] -> List.rev acc
    (* Case: One element, prepend it to the accumulator and continue *)
    | One x::tl -> flat (x::acc) tl
    (* Case: Many elements, flatten the inner list and append it to the accumulator *)
    | Many nl'::tl -> flat (List.rev_append (flat [] nl') acc) tl
  in 
  (* Start the recursion with an empty accumulator *)
  flat [] nl

(* List with one element (One 1) *)
let l1 = [One 1]

(* Empty list *)
let l2 = []

(* List with a nested structure, should flatten to [1;2;3;4] *)
let l3 = [One 1; Many [One 2; One 3; Many [One 4]]]

(* List with strings and nested structure, should flatten to ["a"; "b"; "c"; "d"; "e"] *)
let l4 = [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ]
