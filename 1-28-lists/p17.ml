(*

Split a list into two parts; the length of the first part is given. (easy)

If the length of the first part is longer than the entire list, then the first part is the list and the second part is empty.

*)
(* Function to split a list at the nth element into two sublists *)
let split l n =
  (* Helper recursive function to process the list, accumulating elements in acc and counting the index *)
  let rec aux acc i = function
    (* Base case: if the list is empty, return the reversed accumulator and an empty list *)
    | [] -> List.rev acc, []  
    (* If the index i is less than n, add the current element to the accumulator and continue *)
    | hd::tl when i < n -> aux (hd::acc) (i+1) tl
    (* Otherwise, return the reversed accumulator and the remaining list as the second part *)
    | _ as l -> List.rev acc, l
  in 
  (* Start the recursion with an empty accumulator and index 0 *)
  aux [] 0 l

(* Test case with list ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] and n = 3 *)
(* Should return the first part: ["a"; "b"; "c"], and the second part: ["d"; "e"; "f"; "g"; "h"; "i"; "j"] *)
let l, k = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"], 3

(* Test case with list ["a";"b";"c";"d"] and n = 5 *)
(* Since n is greater than the length of the list, it should return the original list as the first part and an empty list as the second part *)
let l2, k2 = ["a";"b";"c";"d"], 5
