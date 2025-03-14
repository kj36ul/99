(*

Find the last but one (last and penultimate) elements of a list. (easy)

# last_two [ "a" ; "b" ; "c" ; "d" ];;
- : (string * string) option = Some ("c", "d")
# last_two [ "a" ];;
- : (string * string) option = None

*)
(* Recursive function to find the last two elements of a list *)
let rec last_two = function
  (* Case: empty list or list with only one element, return None *)
  | [] | _::[] -> None
  (* Case: list with exactly two elements, return the pair (hd1, hd2) *)
  | hd1::hd2::[] -> Some (hd1, hd2)
  (* Case: list with more than two elements, recursively call last_two on the tail *)
  | hd::tl -> last_two tl

(* Empty list *)
let l1 = []

(* List with only one element *)
let l2 = [1]

(* List with exactly two elements *)
let l3 = [1;2]

(* List with more than two elements *)
let l4 = [1;2;3;4]
