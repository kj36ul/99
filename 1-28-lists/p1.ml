(*

Find the last element of a list.

(Note that the Lisp transcription of this problem is incorrect.)

Example in Haskell:

Prelude> myLast [1,2,3,4]
4
Prelude> myLast ['x','y','z']
'z'

*)


(* Recursive function to find the last element of a list *)
let rec last = function
  (* Case: empty list, return None *)
  | [] -> None
  (* Case: list with only one element, return Some hd *)
  | hd::[] -> Some hd
  (* Case: list with multiple elements, call last on the tail of the list *)
  | hd::tl -> last tl

(* Empty list *)
let l1 = []

(* List with multiple integers *)
let l2 = [1;2;3;4]

(* List with characters *)
let l3 = ['x';'y']
