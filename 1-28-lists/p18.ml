(*

Extract a slice from a list. (medium)

Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included). Start counting the elements with 0 (this is the way the List module numbers elements).

*)
(* Function to slice a list from index i to j (inclusive) *)
let slice l i j =
  (* Helper recursive function to traverse the list while maintaining an accumulator and the current index *)
  let rec aux acc c = function
    (* Base case: if the list is empty, return the reversed accumulator (containing the sliced elements) *)
    | [] -> List.rev acc
    (* If the current index c is less than i, continue traversing without adding the element to the accumulator *)
    | hd::tl ->
      if c < i then aux acc (c+1) tl
      (* If the current index c is between i and j (inclusive), add the element to the accumulator *)
      else if i <= c && c <= j then aux (hd::acc) (c+1) tl
      (* If the current index is greater than j, return the reversed accumulator (elements collected so far) *)
      else List.rev acc
  in 
  (* Start the recursion with an empty accumulator and index 0 *)
  aux [] 0 l

(* Test case with list ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"], i = 2, j = 6 *)
(* Should return the sublist ["c";"d";"e";"f";"g"] (elements at positions 2 through 6 inclusive) *)
let l, i, j = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"], 2, 6
