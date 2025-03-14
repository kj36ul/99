(*
  Replicate the elements of a list a given number of times. (medium)
*)
(* Function to replicate each element in the list n times *)
let replicate l n =
  (* Helper function to replicate x, n times *)
  let xs x n = 
    let rec xs' i acc = 
      if i = n then acc 
      else xs' (i + 1) (x :: acc) 
    in
    xs' 0 []  (* Replicates x n times *)
  in 
  (* Use fold_left to accumulate replicated elements, then reverse to maintain the original order *)
  List.fold_left (fun acc x -> List.rev_append (xs x n) acc) [] l |> List.rev

(* Test case with list ["a";"b";"c"] and n = 3, should return ["a";"a";"a";"b";"b";"b";"c";"c";"c"] *)
let l = ["a"; "b"; "c"]
let l_replicated_3 = replicate l 3 

(* Test case with an empty list and n = 2, should return an empty list [] *)
let l1 = []
let l_replicated_2 = replicate l1 2  
