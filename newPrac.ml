
type square = BOX | EMPTY | BLANK ;;


(* repeat_box returns a list of consecutive SQUARE types *)
let repeat_square (sq : square) (count : int) : (square list) = 
  let repeat_box' sq count acc = 
    match count with 
    | 0 -> acc 
    | x -> repeat_box' sq (count - 1) ([sq] :: acc)
  in 
  repeat_box' sq count [] 
;;

let sum lst = 
  match lst with 
  | [] -> 0
  | h :: t -> h + sum t 
;; 

let rec insert_empty (constraints : int list) (xLen : int) (gap : int): (square list) = 
  match


let get_permutations (contraints : int list) (xLen : int) : (square list list) =
  let tempLen = xLen in 
  match tempLen with 
  | 0 -> 
