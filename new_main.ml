exception Error of string
exception NotFound

type square = BOX | EMP | BLANK ;;

type grid = square list list;;
(* Printing methods *) 

let format_square (sq : square ) : string = 
  if sq = BOX then "■" else if sq = EMP then "□" else " " 
;;

let rec print_arr_int (lst : int list) : unit = 
  match lst with 
  | [] -> Format.print_string("\n") 
  | h :: t -> Format.print_int(h) ; Format.print_string(", ") ; print_arr_int t 
;;  

let rec print_arr_square (lst : square list) : unit = 
  match lst with 
  | [] -> Format.print_string("\n")
  | h :: t -> Format.print_string(format_square h) ; print_arr_square t 
;;

let print_grid (g: grid) = 
    List.iter print_arr_square g;
    ()
;;

(* Methods *)
let rec sum (lst : int list) : int = 
  match lst with
  | [] -> 0 
  | h :: t -> h + sum t
;;

let rec len (lst : 'a list) : int = 
  match lst with 
  | [] -> 0 
  | _ :: t -> 1 + len t 
;;

(* リストの最初の n 要素を取得する関数 *)
let rec take lst n =
    if n <= 0 then []
    else match lst with
         | [] -> []
         | h::t -> h :: take t (n - 1)
;;

(*
  repeat_square
  - repeats a type square n times 
  PARAMS:
  - (sq : square) - what type of squares to complete the array with 
  - (count : int) - how many of type square to fill in. Length of return array.square

  RETURN: 
  - list of filled in squares. Either BOX or EMPTY.
*)
let repeat_square (sq : square) (count : int) : (square list) = 
  let rec repeat_box' sq count acc = 
    match count with 
    | 0 -> acc 
    | _ -> repeat_box' sq (count - 1) (sq :: acc)
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
  generate_all_permutations
  - generates a list of all permutations.
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
      | _ -> failwith "error in generate_single_permutation" 
    in
    let lst = generate_single_permutation' constraints gaps [] in
    let total_len = len lst in
    if total_len > xLen then
      take lst xLen
    else if total_len < xLen then
      lst @ (repeat_square EMP (xLen - total_len))
    else
      lst
  in
  let all_gaps = generate_gaps (len constraints) (xLen - (sum constraints)) in
  let rec generate_permutations' constraints gaps acc = 
    match gaps with  
    | [] -> acc
    | gap :: t -> generate_permutations' constraints t (generate_single_permutation constraints gap :: acc)
  in
  generate_permutations' constraints all_gaps []
;;

(* motherfunction *)
let compute_permutations (constraints : int list) (xLen : int) : square list list = 
  generate_all_permutations constraints xLen
;;

(* ---------- Validity Check ----------*)
(*
  extract_column
  - extracts a column from the grid, including the new row permutation being added 
  PARAMS:
  - (grid : square list list) - nonogram grid built up so far
  - (new_row : square list) - new permutation being added to grid
  - (column_index : int) - index of column in grid

  RETURN:
  - square list - column at column_index of grid
*)

let extract_column grid new_row column_index = 
  let base_column = List.map (fun row -> List.nth row column_index) grid in
  base_column @ [List.nth new_row column_index]
;;

(*
  column_validator
  - checks extracted column against column constraints 
  PARAMS:
  - (partial_column : square list) - column of grid (which is not completed)
  - (constraint : int list) - constraints placed on column of grid

  RETURN:
  - boolean - true if proposed column conforms to constraints, false otherwise
*)

let column_validator (partial_column : square list)  ( cons : int list ) : bool = 
  let rec column_validator_helper constraints current_column current_block = match constraints, current_column with
  | [], [] -> current_block = 0
  | [], EMP :: tl -> column_validator_helper [] tl current_block
  | [], BOX :: _ -> false
  | h :: tlc, BOX :: tl -> 
      let current_block = current_block + 1 in
      if current_block = h then
        column_validator_helper tlc tl 0
      else
        column_validator_helper constraints tl current_block
  | h :: _, EMP :: tl -> 
      if current_block > 0 && current_block <> h then false
      else column_validator_helper constraints tl 0
  | h :: tlc, [] -> 
      if current_block > h then false
      else true
  | _ -> false 
  in
  column_validator_helper cons partial_column 0
;;

(*
  row_validity
  - checks if a given new row permutation will fit with previous rows and column constraints of all columns in grid
  PARAMS:
  - (grid : square list list) - nonogram grid built up so far
  - (new_row : square list) - new permutation being added to grid
  - (column_constraints : int list list) - list of constraints placed on columns of grid

  RETURN:
  - boolean - true if all proposed columns when new row has been added conform to constraints, false otherwise
*)

let row_validity (grid : square list list) (new_row : square list) 
                 (column_constraints : int list list) : bool = 
  let rec validate_columns index = 
    if index >= len column_constraints then true
    else
      let partial_column = extract_column grid new_row index in
      let cons = List.nth column_constraints index in
      if column_validator partial_column cons then validate_columns (index + 1)
      else false
  in
  validate_columns 0
;;

(* dfs function *)
let rec dfs (depth: int) (state: grid) (grid_size: int)
            (horizontal_hints: int list list) (vertical_hints: int list list): bool =
    if depth = grid_size then (
        print_grid state;
        true  
    ) else (
        let possibilities = compute_permutations (List.nth horizontal_hints depth) grid_size in
        if possibilities = [] then (
            false
        ) else (
            let rec try_possibilities poss_list =
                match poss_list with
                | [] -> false  
                | new_row :: rest ->
                    if row_validity state new_row vertical_hints then (
                        let new_state = state @ [new_row] in
                        if dfs (depth + 1) new_state grid_size horizontal_hints vertical_hints then
                            true  
                        else
                            try_possibilities rest
                    ) else
                        try_possibilities rest
            in
            try_possibilities possibilities
        )
    )
;;

(* Test inputs *)
let main () = (

    let vertical_hints = [[3]; [1; 1; 1]; [3]; [1]; [3]] in
    let horizontal_hints = [[4]; [1; 1]; [3; 1]; [1]; [2]] in
    if dfs 0 [] 5 horizontal_hints vertical_hints then
        Printf.printf "Solution found.\n"
    else
        Printf.printf "No solution exists.\n"
)
let () = main ()

