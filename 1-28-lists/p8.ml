(*

Eliminate consecutive duplicates of list elements. (medium)

*)
(* Function to compress a list by removing consecutive duplicate elements *)
let compress l = 
  (* Check if the list is empty, return an empty list if true *)
  if l = [] then []
  (* Use fold_left to traverse the list, accumulating elements without consecutive duplicates *)
  else List.fold_left (fun (acc, x) y -> 
    (* If the current element y is the same as the last one (x), keep the accumulator as is *)
    (* Otherwise, add y to the accumulator and update x to be the new last element *)
    if y = x then acc, x else y::acc, y) 
    (* Initialize the accumulator with the first element of the list and itself as the last element *)
    ([List.hd l], List.hd l) 
    (* Start folding from the second element (List.tl l) *)
    (List.tl l) 
    (* Extract the first element of the result and reverse the list to maintain original order *)
    |> fst |> List.rev

(* Empty list *)
let l1 = []

(* List with consecutive duplicates, should compress to ["a";"b";"c";"a";"d";"e"] *)
let l2 = ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]

(* List with alternating elements, no consecutive duplicates, should remain the same *)
let l3 = [1;2;1;2;1]
