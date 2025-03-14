(*

Create a list containing all integers within a given range. (easy)

If first argument is smaller than second, produce a list in decreasing order.

*)
(* Function to generate a list of integers from i to j (inclusive) *)
let range i j =
  (* Helper recursive function to generate the list of integers between x and y *)
  let rec gen x y acc =
    (* Base case: if x equals y, add x to the accumulator and return the result *)
    if x = y then x::acc
    (* If x is less than y, add y to the accumulator and recursively call gen with y-1 *)
    else if x < y then gen x (y-1) (y::acc)
    (* If x is greater than y, add y to the accumulator and recursively call gen with y+1 *)
    else gen x (y+1) (y::acc)
  in 
  (* Start the recursive generation with an empty accumulator *)
  gen i j []

(* Test case: generate a list of integers from 4 to 9 (inclusive) *)
(* Should result in [4;5;6;7;8;9] *)
let i, j = 4, 9

(* Test case: generate a list of integers from 9 to 4 (inclusive) *)
(* Should result in [9;8;7;6;5;4] *)
let i1, j1 = 9, 4

