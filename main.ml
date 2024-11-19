exception Error of string
exception NotFound

type square = BOX | EMP | BLANK ;;

type grid = square list list;;
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

let print_grid (g: grid) = 
    List.map print_arr_square g;;

(* Methods *)
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
let generate_gaps (num : int)  ( limit : int) : int list list =
  let rec generate ( current_list : int list ) ( remaining_sum : int)  ( remaining_num : int) ( is_first_element : bool ) : int list list =
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

let generate_all_permutations (constraints : int list) (xLen : int) : square list list = 
  let generate_single_permutation (constraints : int list) (gaps : int list ) : square list = 
    let rec generate_single_permutation' constraints gaps (acc : square list) = 
      match constraints, gaps with 
      | [], [] -> acc 
      | con :: x, gap :: y -> 
        generate_single_permutation' x y (acc @ (repeat_square EMP gap) @ (repeat_square BOX con ))
      | _ -> Format.print_string("error \n") ; []
    in
    generate_single_permutation' constraints gaps []
  in
  let all_gaps = generate_gaps (len constraints) (xLen - (sum constraints)) in
  let rec generate_permutations' ( constraints : int list ) (gaps : int list list)  (acc : square list list ) = 
    match gaps with  
    | [] -> acc
    | gap :: t -> generate_permutations' constraints t (acc @ [generate_single_permutation constraints gap])
  in
  generate_permutations' constraints all_gaps []
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
    if len lst = xLen then lst 
    else lst @ repeat_square EMP (xLen - (len lst))
  in
  List.map (fun x -> complete_row x xLen) perms
;;

(* motherfunction *)
let compute_permutations (constraints : int list) (xLen : int) : square list list = 
  let permutations = generate_all_permutations constraints xLen in 
  complete_all_permutations permutations xLen 
;;

(* Taisuke's code from here. *)

let validate (current_state: grid) (horizontal_hints: int list list) = true;;

(*
    dfs
    PARAMS:
        - (depth: int) - the current depth from the root ( 0-indexed )
        - (state: grid) - the current grid
        - (grid_size: int) - the grid size ( N * N )
        - (vertical_hints: int list list) - the hints shown on the left of the Nonogram
        - (horizontal_hints: int list list) - the hints shown on top of the Nonogram

    RETURN:
        - unit. This function doesn't return a specific value.
        - If dfs succeeds, print out the whole grid. If not, throw an excepion NotFound.
 *)

let rec dfs (depth: int) (state: grid) (grid_size: int) (vertical_hints: int list list) (horizontal_hints: int list list): unit =
    let possibilities = compute_permutations (List.nth vertical_hints depth) grid_size in
    if possibilities = [] then raise NotFound
    else
        let rec verify possibilities_list = match possibilities_list with
    | [] -> raise NotFound
    | h::t -> (
        let current_state = List.init depth (fun i -> (List.nth state i)) in
        current_state = current_state@[h];
        current_state = List.append current_state (List.init (grid_size - depth - 1) (fun i ->  (List.init grid_size (fun i -> BLANK))));
        let is_valid = validate current_state horizontal_hints in
        if is_valid = true then (
            if depth = grid_size - 1 then (print_grid current_state; ())
            else dfs (depth + 1) current_state grid_size vertical_hints horizontal_hints
            );
        verify t
        ) in verify possibilities;;


(* Test inputs *)
let main () = (
    Printf.printf "Program started.\n";
)
let () = main ()
