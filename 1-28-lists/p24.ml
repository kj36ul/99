(*

Lotto: Draw N different random numbers from the set 1..M. (easy)

The selected numbers shall be returned in a list.

*)
(* Function to randomly select n distinct numbers from 1 to m *)
let lotto_select n m =
  (* Helper recursive function to perform the selection of n distinct numbers *)
  let rec select i acc =
    (* If we have selected n numbers, return the accumulated list *)
    if i = n then acc
    (* Otherwise, generate a random number r between 1 and m *)
    else 
      let r = Random.int (m-1) + 1 in
      (* If the number r is already in the accumulator, select again *)
      if List.mem r acc then select i acc
      (* Otherwise, add r to the accumulator and continue selecting *)
      else select (i+1) (r::acc)
  in 
  (* Start the selection with index 0 and an empty accumulator list *)
  select 0 []

(* Test case: randomly select 6 distinct numbers from 1 to 49 *)
let n, m = 6, 49
