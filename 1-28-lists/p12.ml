(*

12. Decode a run-length encoded list. (medium)

Given a run-length code list generated as specified in the previous problem 11, construct its uncompressed version.

*)
(* Define a type 'a rle, which can either be a single element (One) or a pair (Many) representing the count and the element *)
type 'a rle =
    | One of 'a           (* Represents a single occurrence of an element *)
    | Many of int * 'a     (* Represents multiple occurrences of an element, using a pair (count, element) *)

(* Function to decode a run-length encoded list back to its original form *)
let decode l = 
  (* Helper recursive function to generate a list of repeated elements based on the count *)
  let rec duplicates a n i acc =
    (* Base case: if the index (i) reaches the count (n), return the accumulated list *)
    if i = n then acc
    (* Recursive case: add the element a to the accumulator until we reach the count n *)
    else duplicates a n (i+1) (a::acc)
  in 
  (* Fold over the encoded list and process each element based on whether it's a One or Many *)
  List.fold_left (fun acc x -> 
    match x with 
    (* Case: If it's a single occurrence (One x), just add x to the accumulator *)
    | One x -> x::acc 
    (* Case: If it's multiple occurrences (Many (c, x)), generate c repetitions of x and add them to the accumulator *)
    | Many (c, x) -> List.rev_append (duplicates x c 0 []) acc) [] l 
  (* Reverse the final list to maintain the original order after folding *)
  |> List.rev

(* List with run-length encoded elements, should decode to ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] *)
let l = [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")]

(* Empty list *)
let l1 = []
