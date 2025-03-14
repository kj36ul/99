(*

Group the elements of a set into disjoint subsets. (medium)

1. In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? Write a function that generates all the possibilities and returns them in a list.

2. Generalize the above function in a way that we can specify a list of group sizes and the function will return a list of groups

*)
(* Define the Cannot_group exception, which will be raised if grouping is not possible *)
  exception Cannot_group

(* Function to calculate the sum of elements in a list l *)
let sum l = List.fold_left (+) 0 l

(* Function to extract k elements from the list l and return them as groups *)
let extract k l = 
  (* Helper recursive function to extract groups *)
  let rec extr rest k = function
  | [] -> []  (* If the list is empty, return an empty list *)
  | _ when k = 0 -> []  (* If k is 0, return an empty list *)
  | hd::tl when k = 1 ->  (* When k is 1, create a group with the current element *)
    ([hd], rest @ tl) :: (extr (hd::rest) 1 tl)  (* Group the head element with the rest of the list *)
  | hd::tl ->  (* Otherwise, recursively generate groups with the head element and the tail *)
    List.map (fun x -> hd :: (fst x), (snd x)) (extr rest (k - 1) tl) 
    |> List.append (extr (hd::rest) k tl)  (* Append the groups for k-1 and continue recursively *)
  in 
  extr [] k l  (* Start extraction with an empty accumulator *)

(* Function to group elements of the list l according to the list kl of group sizes *)
let group l kl =
  (* Check if the total number of elements in l is less than the sum of the sizes in kl *)
  if List.length l < sum kl then raise Cannot_group  (* If so, raise Cannot_group exception *)
  else 
    (* Helper recursive function to generate the groups for each size in kl *)
    let rec grp l = function
      | [] -> [[]]  (* If there are no more sizes to process, return an empty list of lists *)
      | k::tl ->  (* For each group size k in kl *)
        (* Extract k elements from l and then recursively group the rest of the list *)
        extract k l 
        |> List.map (fun (cs, rest) -> 
            grp rest tl 
            |> List.map (fun y -> cs :: y))  (* Generate the final grouping by prepending cs to each group *)
        |> List.flatten  (* Flatten the resulting list of lists *)
    in 
    grp l kl  (* Start grouping from the full list and the size list kl *)

(* Test case: group the list [1;2;3;4] into groups of sizes [2;1] *)
let l, kl = [1;2;3;4], [2;1]
