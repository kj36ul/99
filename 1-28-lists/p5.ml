(*

Reverse a list. (easy)

OCaml standard library has List.rev but we ask that you reimplement it.

*)

(* Function to reverse a list using tail recursion *)
let rev l =
  (* Helper recursive function to accumulate the reversed list *)
  let rec rev' acc = function
    (* Case: empty list, return the accumulated reversed list *)
    | [] -> acc
    (* Case: non-empty list, prepend the head to the accumulator and recurse on the tail *)
    | hd::tl -> rev' (hd::acc) tl
  in 
  (* Start the recursion with an empty accumulator *)
  rev' [] l

(* Empty list *)
let l1 = []

(* List with three elements *)
let l2 = [1;2;3]
