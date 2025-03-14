(*

Run-length encoding of a list. (easy)

If you need so, refresh your memory about run-length encoding.

*)
(* Function to encode a list by representing consecutive duplicates as pairs (count, element) *)
let encoding l =
  (* Check if the list is empty, return an empty list if true *)
  if l = [] then [] 
  (* Otherwise, process the list by accumulating pairs (count, element) for consecutive duplicates *)
  else 
    (* Use fold_left to accumulate counts of consecutive duplicates and form (count, element) pairs *)
    let acc, s = List.fold_left (fun (acc, (c, x)) y -> 
      (* If the current element y is the same as the last one (x), increment the count (c) *)
      (* Otherwise, store the current pair (c, x) and start a new count for y *)
      if y = x then acc, (c + 1, x) 
      else (c, x) :: acc, (1, y)) 
    (* Initialize the accumulator with the first element of the list and a count of 1 *)
    ([], (1, List.hd l)) (List.tl l)
    in 
    (* Reverse the accumulator and prepend the last pair (s) *)
    List.rev (s :: acc)

(* List with consecutive duplicates, should encode to [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")] *)
let l1 = ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]

(* Empty list *)
let l2 = []

(* List with alternating elements, should encode to [(1, 1); (1, 2); (2, 2); (1, 3); (1, 1); (1, 4)] *)
let l3 = [1;2;2;3;1;4]
