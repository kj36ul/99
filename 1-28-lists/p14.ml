(*
  Duplicate the elements of a list. (easy)
*)

(* Function to duplicate each element in the list *)
let duplicate l =
  (* Use fold_left to traverse the list, adding each element twice to the accumulator *)
  List.fold_left (fun acc x -> x::x::acc) [] l 
  (* Reverse the list at the end to maintain the original order *)
  |> List.rev

(* Empty list, should return an empty list [] *)
let l1 = []

(* List with some duplicates, should return ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"] *)
let l2 = ["a";"b";"c";"c";"d"]
