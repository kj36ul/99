(*

13. Run-length encoding of a list (direct solution). (medium)

Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem "Pack consecutive duplicates of list elements into sublists", but only count them. As in problem "Modified run-length encoding", simplify the result list by replacing the singleton lists (1 X) by X.

    # encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
    - : string rle list = [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]

*)
(* Define a type 'a rle, which can either be a single occurrence (One) or a pair representing the count and the element (Many) *)
type 'a rle = One of 'a | Many of int * 'a

(* Function to encode a list using run-length encoding (RLE) *)
let encode l =
  (* Check if the list is empty, return an empty list if true *)
  if l = [] then []
  else 
    (* Helper function to add an element to the result list, choosing between One or Many based on the count (c) *)
    let add (c, x) l = if c = 1 then One x :: l else Many (c, x) :: l in
    (* Use fold_left to traverse the list and accumulate counts of consecutive duplicates *)
    let acc, s = 
      List.fold_left (fun (acc, (c, x)) y -> 
        (* If the current element y is the same as the last one (x), increment the count (c) *)
        (* Otherwise, add the previous (c, x) pair to the accumulator and start a new pair for y *)
        if y = x then acc, (c + 1, x) 
        else add (c, x) acc, (1, y)) 
      (* Initialize the accumulator with the first element of the list and a count of 1 *)
      ([], (1, List.hd l)) (List.tl l)
    in
    (* Add the last (count, element) pair and reverse the list to maintain the original order *)
    List.rev (add s acc)

(* List with consecutive duplicates, should encode to [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")] *)
let l = ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]
