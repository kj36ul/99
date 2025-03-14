(*

Generate the combinations of K distinct objects chosen from the N elements of a list. (medium)

In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients). For pure mathematicians, this result may be great. But we want to really generate all the possibilities in a list.

*)

(* remove all the duplicating elements, ignoring the order *)
(* Function to remove duplicate elements from the list l *)
let rm_dup l =
  (* If the list is empty, return an empty list *)
  if l = [] then []
  else 
    (* Sort the list to bring duplicates together *)
    let sl = List.sort compare l in
    (* Get the first element of the sorted list *)
    let hd = List.hd sl in
    (* Fold through the list, accumulating elements and removing duplicates *)
    List.fold_left (fun (acc,x) y -> 
      (* If the current element is equal to the previous one, skip it *)
      if y = x then acc,x 
      (* Otherwise, add the current element to the accumulator *)
      else y::acc,y) ([hd],hd) (List.tl sl) 
    |> fst  (* Return the first part of the tuple, which is the list without duplicates *)

(* Function to extract all combinations of length k from the list l *)
let extract k l =
  (* Remove duplicates from the list before generating combinations *)
  let no_dup = rm_dup l in
  (* Helper recursive function to generate combinations of length k *)
  let rec extr i = function
    (* If the list is empty, return an empty list *)
    | [] -> []
    (* If i = 0, return an empty list *)
    | _::_ when i = 0 -> []
    (* When we reach i = 1, return a list of single-element lists from the remaining elements *)
    | hd::tl when i = 1 -> List.map (fun x -> [x]) (hd::tl)
    (* Otherwise, generate combinations of length (i-1) for each tail element *)
    | hd::tl -> List.map (fun x -> hd::x) (extr (i-1) tl) 
    (* Append the combinations of length k from the remaining list *)
    |> List.append (extr k tl)
  in 
  (* Start extracting combinations with length k from the list without duplicates *)
  extr k no_dup

(* Test case: extract combinations of length 2 from the list ["a";"b";"c";"d";"a"] *)
let k,l = 2, ["a";"b";"c";"d";"a"]
