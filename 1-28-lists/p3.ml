(*

Find the k'th element of a list. (easy)

# at 3 [ "a" ; "b"; "c"; "d"; "e" ];;
- : string option = Some "c"
# at 3 [ "a" ];;
- : string option = None

*)

(* Recursive function to find the element at position k in a list *)
let rec at k = function
  (* Case: empty list, return None *)
  | [] -> None
  (* Case: if k is 0, return the head of the list wrapped in Some *)
  | hd::tl when k = 0 -> Some hd
  (* Case: decrease k by 1 and recursively call at on the tail of the list *)
  | hd::tl -> at (k-1) tl

(* Variable k1 is set to 3 and l1 is set to a list with one element [1] *)
let k1,l1 = 3,[1]

(* Variable k2 is set to 3 and l2 is set to a list [1;2;3;4] *)
let k2,l2 = 3,[1;2;3;4]
