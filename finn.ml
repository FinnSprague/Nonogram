exception Error

type square = BOX | EMP | BLANK ;;
let rec sum (lst : int list) : int = 
  match lst with
  | [] -> 0 
  | h :: t -> h + sum t
;;

let rec len (lst : 'a list) : int = 
  match lst with 
  | [] -> 0 
  | h :: t -> 1 + len t 
;;

let head l = match l with
| [] -> raise Error
| h::t -> h
;;

(* 
  repeat_square
  - repeats a type square n times 
  PARAMS:
  - (sq : sqare) - what type of squares to complete the array with 
  - (count : int) - how many of type square to fill in. Length of return array.square

  RETURN: 
  - list of filled in squares. Either BOX or EMPTY.
*)
let repeat_square (sq : square) (count : int) : (square list) = 
  let rec repeat_box' sq count acc = 
    match count with 
    | 0 -> acc 
    | x -> repeat_box' sq (count - 1) ([sq] @ acc)
  in 
  repeat_box' sq count [] 
;;

(* 
  generate_gaps
  - generates a numerical list of lists representing the length of gaps in every permutation
  PARAMS:
  - (num : int) - number of constraints 
  - (limit : int) - total number of gaps cannot exceed the limit. Normally given by (length of grid) - (sum of constraints)

  RETURN: 
  - a list of list of lengths of all gaps
*)
let generate_gaps num limit  =
  let rec generate current_list remaining_sum remaining_num is_first_element =
    if remaining_num = 0 then
      if remaining_sum >= 0 then [List.rev current_list] else []
    else
      let min_value = if is_first_element then 0 else 1 in
      let rec try_value value acc =
        if value > remaining_sum then acc
        else
          let new_list = generate (value :: current_list) (remaining_sum - value) (remaining_num - 1) false in
          try_value (value + 1) (acc @ new_list)
      in
      try_value min_value []
  in
  generate [] limit num true
;;

(* 
  generate_permutations
  - generates a list of all permutations. Some might be incomplete and all are reversed.
  PARAMS:
  - (constraints : int list) - numerical constraints on row 
  - (xLen : int) - length of grid

  RETURN: 
  - all permutations as a square list
*)
let generate_permutations (constraints : int list) (xLen : int) : square list list =
  let all_gaps = generate_gaps (len constraints)  ( xLen - (sum constraints)) in
  let rec generate_permutations' constraints all_gaps acc =
    match constraints, all_gaps with
    | [], [] -> [List.rev acc]  
    | con :: rest_con, gap :: rest_gap ->
      let new_acc = acc @ (repeat_square EMP gap) @ (repeat_square BOX con) in
      generate_permutations' rest_con rest_gap new_acc
    | _ -> Format.print_string "error"; [[BLANK]] 
  in
  (* Generate permutations for each gap configuration *)
  List.flatten (List.map (fun gaps -> List.rev (generate_permutations' constraints gaps [])) all_gaps)
;;


(* 
  complete_all_permutations
  PARAMS:
  - (perms : square list list) - all permutations 
  - (xLen : int) - length of grid
  
  RETURN: 
  - a complete list of rows, unreversed and filled. 
*)
let complete_all_permutations (perms : square list list) (xLen : int): square list list = 
  let complete_row (lst : square list) (xLen : int) : square list = 
    if len lst = xLen then List.rev lst 
    else List.rev (lst @ repeat_square EMP (xLen - (len lst)))
  in
  List.map (fun x -> complete_row x xLen) perms 
;;

(* motherfunction *)
let compute_permutations (constraints : int list) (xLen : int) : square list list = 
  let permutations = generate_permutations constraints xLen in 
  complete_all_permutations permutations xLen 
;;





(* Printing methods *) 

let format_square (sq : square ) : string = 
  if sq = BOX then "BOX" else if sq = EMP then "EMP" else "BLANK" 
;;

let rec print_arr_int (lst : int list) : unit = 
  match lst with 
  | [] -> Format.print_string("\n") 
  | h :: t -> Format.print_int(h) ; Format.print_string(", ") ; print_arr_int t 
;;  

let rec print_arr_square (lst : square list) : unit = 
  Format.print_string(" | ") ; 
  match lst with 
  | [] -> Format.print_string("\n \n")
  | h :: t -> Format.print_string(format_square h) ; print_arr_square t 
;;


(* Test inputs *)
let hints = [3; 2; 1] in
let generated = compute_permutations hints 10 in
let rec check permutation_arr = 
    match permutation_arr with
    | [] -> ()
    | h::t -> (
        print_arr_square h;
        check t
    ) in check generated;;
