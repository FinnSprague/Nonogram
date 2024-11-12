

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

let repeat_square (sq : square) (count : int) : (square list) = 
  let rec repeat_box' sq count acc = 
    match count with 
    | 0 -> acc 
    | x -> repeat_box' sq (count - 1) ([sq] @ acc)
  in 
  repeat_box' sq count [] 
;;

let generate_gaps num limit  =
  (* Recursive function to build lists with constraints *)
  let rec generate current_list remaining_sum remaining_num is_first_element =
    if remaining_num = 0 then
      (* If we reached the required length, return the list if it satisfies the sum constraint *)
      if remaining_sum >= 0 then [List.rev current_list] else []
    else
      (* Determine the minimum value for the current element *)
      let min_value = if is_first_element then 0 else 1 in
      (* Try each possible value for the current element *)
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


let complete_all_permutations (perms : square list list) (xLen : int): square list list = 
  let complete_row (lst : square list) (xLen : int) : square list = 
    if len lst = xLen then List.rev lst 
    else List.rev (lst @ repeat_square EMP (xLen - (len lst)))
  in
  List.map (fun x -> complete_row x xLen) perms 
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


let clues = [3;2;1];; 

let generated = complete_all_permutations (generate_permutations clues 10) 10 ;; 
let gaps = generate_gaps (len [3;2;1;]) (10 - sum [3;2;1;]) ;; 
List.iter (fun x -> print_arr_square x) generated ;; 

 
