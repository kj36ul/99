(*

Pack consecutive duplicates of list elements into sublists. (medium)

*)
(* Function to pack consecutive duplicates in a list into sublists *)
let pack l =
  (* Check if the list is empty, return an empty list if true *)
  if l = [] then [] 
  (* Otherwise, continue processing the list *)
  else 
    (* Get the first element of the list (head) *)
    let hd = List.hd l in
    (* Use fold_left to traverse the list, accumulating sublists of consecutive duplicates *)
    let acc, (sub, _) = 
      List.fold_left (fun (acc, (sub, x)) y -> 
        (* If the current element y is the same as the last one (x), add y to the current sublist *)
        (* Otherwise, add the current sublist to the accumulator and start a new sublist with y *)
        if y = x then acc, (y :: sub, x) 
        else sub :: acc, ([y], y)) 
      (* Initialize the accumulator with an empty list and the first element as the first sublist *)
      ([], ([hd], hd)) (List.tl l) 
    in
    (* Reverse the accumulator and prepend the last sublist (sub) to maintain original order *)
    List.rev (sub :: acc)

(* List with consecutive duplicates, should pack into sublists like [["a";"a";"a";"a"]; ["b"]; ["c";"c"]; ["a";"a"]; ["d";"d"]; ["e";"e";"e";"e"]] *)
let l1 = ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"]

(* Empty list *)
let l2 = []

(* List with alternating elements, should pack as [["1"]; ["2"]; ["1"]; ["3"]; ["2"]; ["1"]; ["1"]] *)
let l3 = [1;2;1;3;2;1;1]
