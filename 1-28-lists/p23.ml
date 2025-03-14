(*

Extract a given number of randomly selected elements from a list. (medium)

The selected items shall be returned in a list. We use the Random module but do not initialize it with Random.self_init for reproducibility.

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
let rand_select l n =
  (* Helper recursive function to perform the random selection of n elements *)
  let rec select i acc l len =
    (* If we have selected n elements, return the accumulated list of selected elements *)
    if i = n then acc
    (* Otherwise, randomly extract an element and continue selecting *)
    else 
      (* Use the extract function to select a random element from the list *)
      let s, l = extract (Random.int len) l in
      select (i+1) (s::acc) l (len-1)
  in 
  (* Start the selection with index 0, an empty accumulator, and the length of the list *)
  select 0 [] l (List.length l) |> List.rev

(* Test case: randomly select 3 elements from the list ["a";"b";"c";"d";"e";"f";"g";"h"] *)
let l, k = ["a";"b";"c";"d";"e";"f";"g";"h"], 3
