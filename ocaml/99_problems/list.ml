(* utop >>= #use "list.ml" *)


(* 1. return the last element *)
let rec last = function
  | [] -> None
  | [x] -> Some x
  | _ :: t -> last t;;

(* 2. find the last but one element of a list *)
let rec last_two = function
  | [] | [_] -> None
  | [x;y] -> Some(x, y)
  | _::t -> last_two t;;

(* 3. find the kth element of a list *)
let rec at k = function
  | [] -> None
  | h :: t -> if k = 1 then Some h else at (k - 1) t;;

(* 4. find the number of elements of a list *)
let rec len = function
  | [] -> 0
  | h :: t -> 1 + len t;;
(* tail-recursive version *)
let length list =
  let rec aux n = function
    | [] -> n
    | _::t -> aux (n + 1) t
  in aux 0 list;;

(* 5. reverse a list *)
let rec rev list = 
  let rec aux acc = function
    | [] -> acc
    | h::t -> aux (h::acc) t in
  aux [] list;;

(* 6. finds out whether a list is a palindrome *)
let is_palindrome list = 
  list = rev list
