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
    List.map print_arr_square g;
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


(* ---------- Ayo's Validity Check ----------*)
(* 
  extract_column
  - extracts a column from the grid, including the new row permutation being added 
  PARAMS:
  - (grid : square list list) - nongram grid built up so far
  - (new_row : square list) - new permutation being added to grid
  - (column_index : int) - index of column in grid
  
  
  RETURN:
  - square list - column at column_index of grid
*)

let rec extract_column grid new_row column_index = 
  let base_column = List.map(fun row -> List.nth row column_index) grid in
  base_column @ [List.nth new_row column_index]
;;

(* 
  column_constraint_check
  - checks extracted column against column constraints 
  PARAMS:
  - (partial_column : square list) - column of grid (which is not completed)
  - (constraint : int list) - constraints placed on column of grid
  
  RETURN:
  - boolean - true if proposed column conforms to constraints, false otherwise
*)

let column_validator (partial_column : square list)  ( cons : int list ) : bool = 
  let rec column_validator_helper constraints current_column current_block = match constraints, current_column with
  |[], [] -> current_block = 0
  |[], EMP :: tl -> column_validator_helper [] tl current_block
  |[], BOX :: _ -> false
  |h :: tlc, BOX :: tl -> if current_block + 1 = h then column_validator_helper tlc tl 0
  else column_validator_helper constraints tl (current_block + 1)
  |h :: _, EMP:: tl -> if current_block > 0 then false else column_validator_helper constraints tl 0
  |h :: _, [] when current_block > 0 -> current_block = h
  |_, [] -> true
  |_ -> false 
in
column_validator_helper cons partial_column 0

;;

(* 
  row_validity
  - checks if a given new row permutation will fit with previous rows and column constraints of all columns in grid
  PARAMS:
  - (grid : square list list) - nongram grid built up so far
  - (new_row : square list) - new permutation being added to grid
  - (column_constraints : int list list) - list of constraints placed on columns of grid
  
  RETURN:
  - boolean - true if all proposed columns when new row has been added conform to constraints, false otherwise
*)

let row_validity (grid : square list list) (new_row : square list) 
                 (column_constrains : int list list) : bool = 
  let rec validate_columns index = 
    if index >= List.length column_constrains then true
    else
      let partial_column = extract_column grid new_row index in
      let cons = List.nth column_constrains index in
      if column_validator partial_column cons then validate_columns (index + 1)
      else false
in
validate_columns 0
;;

(* Taisuke's dfs *)

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
    print_grid state;
    let possibilities = compute_permutations (List.nth vertical_hints depth) grid_size in
    if possibilities = [] then Printf.printf "No possibilities\n"
    else
        let rec verify possibilities_list = match possibilities_list with
    | [] -> Printf.printf "Run out of possibilities."
    | h::t -> (
        Printf.printf "Hoge\n";
        let is_valid = row_validity state h horizontal_hints in
        if is_valid then (
            let new_state = List.init depth (fun i -> (List.nth state i)) in
            new_state = new_state@[h];
            if depth = grid_size - 1 then print_grid new_state
            else (
                Printf.printf "Fuga\n";
                dfs (depth + 1) new_state grid_size vertical_hints horizontal_hints
            )
        );
        verify t
    ) in verify possibilities
;;

(* Test inputs *)
let main () = (

    let vertical_hints = [[3]; [1; 1; 1]; [3]; [1]; [3]] in
    let horizontal_hints = [[4]; [1; 1]; [3; 1]; [1]; [2]] in
    if row_validity [] [BOX; BOX; BOX; EMP; EMP] horizontal_hints then Printf.printf "Hoge\n";
    ()
)
let () = main ()
