type square = BOX | EMP ;;
 

(* Permutations *) 

let rec repeat_square (sq : square) (n : int): square list ;;
let rec generate_list_of_gaps (constraints : int list) (gridLen : int) : int list;; 

let generate_all_permutations (constraints :int list) (gridLen) : square list list = 
  let rec compute_permutation (constraints : int list ) (gaps : int list) (gridLen : int) : square list =
    match gaps, constraints with 
    | [], [] -> []
    | h :: t, h' :: t'  -> repeat_square EMP h @ repeat_square BOX h' @ compute_permutation t t' gridLen 
  and gaps = generate_list_of_gaps constraints gridLen in 
  let rec generate_all_permutations' (gaps : int list list) ( gridLen : int ) : square list list =
    match gaps with 
    | [] -> []
    | h :: t -> (compute_permutation h gridLen) @ generate_all_permutations' t gridLen
  in 
  generate_all_permutations' gaps gridLen 
;;
  



