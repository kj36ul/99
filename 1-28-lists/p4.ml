(*

Find the number of elements of a list. (easy)

OCaml standard library has List.length but we ask that you reimplement it. Bonus for a tail recursive solution.

*)

(* Function to calculate the length of a list using tail recursion *)
let length_tail l = 
  (* Helper recursive function to accumulate the length of the list *)
  let rec len acc = function
    (* Case: empty list, return the accumulated length *)
    | [] -> acc
    (* Case: non-empty list, increment the accumulator and recurse on the tail *)
    | _::tl -> len (acc+1) tl
  in 
  (* Start the recursion with an accumulator of 0 *)
  len 0 l

(* Empty list *)
let l1 = []

(* List with three elements *)
let l2 = [1;2;3]

