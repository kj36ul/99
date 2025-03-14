(*

##### 28. Sorting a list of lists according to length of sublists. (medium)

1. We suppose that a list contains elements that are lists themselves. The objective is to sort the elements of this list according to their length. E.g. short lists first, longer lists later, or vice versa.

2. Again, we suppose that a list contains elements that are lists themselves. But this time the objective is to sort the elements of this list according to their length frequency; i.e., in the default, where sorting is done ascendingly, lists with rare lengths are placed first, others with a more frequent length come later.

*)
(* Function to sort a list of lists based on the length of each list *)
let length_sort l =
  (* Map each element (sublist) to a tuple of the sublist and its length *)
  List.map (fun x -> (x, List.length x)) l 
  (* Sort the list of tuples by comparing the second element (length) of each tuple *)
  |> List.sort (fun x y -> compare (snd x) (snd y)) 
  (* Map back to only the sublists, removing the length *)
  |> List.map fst

(* Helper function to compare two tuples based on the second element of each tuple *)
let compare_snd x y = compare (snd x) (snd y)

(* Function to sort a list of lists based on the frequency of each sublist length in the list *)
let frequency_sort l =
  (* First, create a list of tuples where each element is paired with its length *)
  let sl = List.map (fun x -> (x, List.length x)) l 
  (* Sort the list of tuples based on the length (using the compare_snd function) *)
  |> List.sort compare_snd in
  (* Extract the first sublist and its length from the sorted list *)
  let hd, hd_len = List.hd sl in
  (* Use List.fold_left to accumulate sublists that have the same length and their counts *)
  let acc, sub, c, _ = List.fold_left 
    (fun (acc, sub, c, len1) (x, len2) -> 
      (* If the current sublist length is equal to the previous one, accumulate it *)
      if len2 = len1 then acc, x :: sub, c + 1, len1 
      (* Otherwise, store the accumulated sublist and start a new one for the new length *)
      else (sub, c) :: acc, [x], 1, len2
    )
    (* Initialize the fold with the first element of the sorted list *)
    ([], [hd], 1, hd_len) 
    (List.tl sl) in
  (* Append the final sublist group to the accumulator *)
  let l2 = (sub, c) :: acc in
  (* Sort the groups again by their length and flatten the resulting list of sublists *)
  List.sort compare_snd l2 
  |> List.map (fun (sub, _) -> sub)  (* Map to only the sublists, ignoring the counts *)
  |> List.flatten  (* Flatten the list of sublists into a single list *)

(* Test case: A list of lists of strings *)
let l = [ ["a"; "b"; "c"]; ["d"; "e"]; ["f"; "g"; "h"]; ["d"; "e"]; ["i"; "j"; "k"; "l"]; ["m"; "n"]; ["o"] ]
