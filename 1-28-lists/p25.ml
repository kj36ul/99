(*

Generate a random permutation of the elements of a list. (easy)

*)
(* Function to extract the k-th element from the list l *)
let extract k l = 
  (* Helper recursive function to traverse the list and extract the k-th element *)
  let rec extr i acc = function
    (* If the list is empty, raise a Not_found exception *)
    | [] -> raise Not_found
    (* If we reach the k-th element, return it and the remaining list *)
    | hd::tl when i = k -> hd, List.rev_append acc tl
    (* Otherwise, continue recursively, accumulating elements in acc *)
    | hd::tl -> extr (i+1) (hd::acc) tl
  in 
  (* Start the extraction with index 0 and an empty accumulator *)
  extr 0 [] l

(* Function to randomly select n elements from the list l *)
let rand_select n l =
  (* Helper recursive function to perform the random selection of n elements *)
  let rec select acc i l len = 
    (* If we have selected n elements, return the accumulated list *)
    if i = n then acc
    (* Otherwise, randomly extract an element and continue selecting *)
    else 
      (* Use the extract function to select a random element from the list *)
      let r,l' = extract (Random.int len) l in
      (* Continue recursively, accumulating the selected elements *)
      select (r::acc) (i+1) l' (len-1)
  in 
  (* Start the selection with an empty accumulator, index 0, and the length of the list *)
  select [] 0 l (List.length l)

(* Function to generate a random permutation of the list l *)
let permutation l = rand_select (List.length l) l

(* Test case: generate a random permutation of the list ["a";"b";"c";"d";"e";"f"] *)
let l = ["a"; "b"; "c"; "d"; "e"; "f"]

