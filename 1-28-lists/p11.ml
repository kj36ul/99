(*

11. Modified run-length encoding. (easy)

Modify the result of the previous problem in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.

Since OCaml lists are homogeneous, one needs to define a type to hold both single elements and sub-lists.

*)
(* Define a type 'a rle which can either be a single element (One) or a pair (Many) representing the count and the element *)
type 'a rle = One of 'a | Many of int * 'a

(* Function to encode a list using run-length encoding (RLE) *)
let encode l =
  (* Check if the list is empty, return an empty list if true *)
  if l = [] then []
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
    (* Map the (count, element) pairs to the 'a rle type: use One for single occurrences and Many for multiples *)
    List.map (fun (c, x) -> if c = 1 then One x else Many (c, x)) (s :: acc)

(* List with consecutive duplicates, should encode to [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")] *)
let l = ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]

(* Empty list *)
let l1 = []

(* List with no consecutive duplicates, should encode to [One 1; One 2; One 3] *)
let l2 = [1;2;3]
