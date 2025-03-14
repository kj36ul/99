(*

Find out whether a list is a palindrome. (easy)

HINT: a palindrome is its own reverse.

*)

(* Function to check if a list is a palindrome *)
  let is_palindrome l = 
    (* Check if the list is non-empty and equal to its reverse *)
    l <> [] && l = List.rev l
  
  (* List [1;2;3] is not a palindrome *)
  let l1 = [1;2;3]
  
  (* List [1;2;3;2;1] is a palindrome *)
  let l2 = [1;2;3;2;1]
  
  (* Empty list is considered a palindrome by the function *)
  let l3 = []
  