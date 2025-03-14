(*
  Drop every N'th element from a list. (medium)
*)

(* Function to drop the nth element from a list *)
let drop l n =
  (* Use fold_left to traverse the list, maintaining an accumulator and index (i) *)
  List.fold_left (fun (acc, i) x -> 
    (* If the current index i matches n, skip the element (do not add it to the accumulator) *)
    (* Otherwise, add the element x to the accumulator *)
    if i = n then acc, i + 1 
    else x :: acc, i + 1) 
  (* Initialize the accumulator as an empty list and start with index 1 *)
  ([], 1) l 
  (* After the fold, retrieve the accumulated list (acc), reverse it to maintain the original order, and return the result *)
  |> fst 
  |> List.rev

(* List with elements ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] and n = 3 *)
let l, k = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"], 3
