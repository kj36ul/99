(*

Rotate a list N places to the left. (medium)

*)
(* Function to rotate a list by n positions *)
let rotate l n =
  (* Helper recursive function to "roll" the list, separating it into two parts: the first part (acc) and the rest of the list *)
  let rec roll acc i k = function
    (* Base case: if the list is empty, return the accumulated list and an empty list *)
    | [] -> acc, []
    (* When the index i matches k, split the list into two parts: acc and the remaining tail *)
    | _::tl when i = k -> acc, tl
    (* Otherwise, continue recursively, adding the current element to acc and incrementing the index i *)
    | hd::tl -> roll (hd::acc) (i+1) k tl
  in 
  (* If n is negative, reverse the list, perform the roll, and then reverse the result back to maintain the original order *)
  if n < 0 then
    let a, b = roll [] 0 (abs n) (List.rev l) in 
    (* Reverse both parts and append them to get the rotated list *)
    List.rev_append (List.rev a) (List.rev b)
  (* If n is non-negative, perform the roll normally and return the rotated list *)
  else
    let a, b = roll [] 0 n l in
    (* Reverse the two parts and append them to form the rotated list *)
    List.rev_append (List.rev b) (List.rev a)

(* Test case with list ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] and n = 3 *)
(* Should rotate the list 3 positions to the left, resulting in ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"] *)
let l, n = ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"], 3

(* Test case with list ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] and n = -2 *)
(* Should rotate the list 2 positions to the right, resulting in ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"] *)
let l1, n1 = ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"], (-2)
