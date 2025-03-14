(*

Insert an element at a given position into a list. (easy)

Start counting list elements with 0. If the position is larger or equal to the length of the list, insert the element at the end. (The behavior is unspecified if the position is negative.)

*)
(* Function to insert an element x at the kth position in the list l *)
let insert_at x k l =
  (* Helper recursive function to traverse the list and insert x at the kth position *)
  let rec aux acc i = function
    (* Base case: if the list is empty, append x to the accumulator and return it as a single-element list *)
    | [] -> List.rev_append acc [x]
    (* If the current index i matches k, insert x before the current head and append the rest of the list *)
    | hd::tl when i = k -> List.rev_append (x::acc) (hd::tl)
    (* Otherwise, continue traversing the list, accumulating elements and incrementing the index *)
    | hd::tl -> aux (hd::acc) (i+1) tl
  in 
  (* Start the recursion with an empty accumulator and index 0 *)
  aux [] 0 l

(* Test case: insert "alfa" at index 1 in the list ["a";"b";"c";"d"] *)
(* Should result in ["a"; "alfa"; "b"; "c"; "d"] *)
let x, k, l = "alfa", 1, ["a";"b";"c";"d"]

(* Test case: insert "alfa" at index 3 in the list ["a";"b";"c";"d"] *)
(* Should result in ["a"; "b"; "c"; "alfa"; "d"] *)
let x1, k1, l1 = "alfa", 3, ["a";"b";"c";"d"]

(* Test case: insert "alfa" at index 4 in the list ["a";"b";"c";"d"] *)
(* Should result in ["a"; "b"; "c"; "d"; "alfa"] *)
let x2, k2, l2 = "alfa", 4, ["a";"b";"c";"d"]
