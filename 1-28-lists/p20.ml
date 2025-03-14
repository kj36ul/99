(*

Remove the K'th element from a list. (easy)

The first element of the list is numbered 0, the second 1,...

*)
(* Function to remove the element at the kth position in a list *)
let remove_at kth l =
  (* Helper recursive function to traverse the list while maintaining an accumulator and current index *)
  let rec aux acc i = function
    (* Base case: if the list is empty, return the original list (this won't happen due to pattern matching) *)
    | [] -> l
    (* If the current index i matches kth-1, remove the current element and append the rest of the list to the accumulator *)
    | hd::tl when i = kth-1 -> List.rev_append acc tl
    (* Otherwise, continue traversing the list, accumulating elements and incrementing the index *)
    | hd::tl -> aux (hd::acc) (i+1) tl
  in 
  (* Start the recursion with an empty accumulator and index 0 *)
  aux [] 0 l

(* Test case with k = 1 and list ["a";"b";"c";"d"] *)
(* Should remove the element at index 1 (0-based index), resulting in ["b";"c";"d"] *)
let k, l = 1, ["a";"b";"c";"d"]
