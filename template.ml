type square = BOX | EMP ;;
type grid = square list list ;; 
exception Error of string
exception NotFound
exception NotImplemented


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

let rec take lst n =
  raise NotImplemented
;;


(* Permutations *) 


(* 
  repeat_square
  - repeats a type square n times 
  PARAMS:
  - (sq : sqare) - what type of squares to complete the array with 
  - (count : int) - how many of type square to fill in. Length of return array.square

  RETURN: 
  - list of filled in squares. Either BOX or EMPTY.
*)
let rec repeat_square (sq : square) (n : int): square list =
  raise NotImplemented
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
    raise NotImplemented 
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
      raise NotImplemented
    in
    generate_single_permutation' constraints gaps []
  in
  let all_gaps = generate_gaps (len constraints) (xLen - (sum constraints)) in
  let rec generate_permutations' ( constraints : int list ) (gaps : int list list)  (acc : square list list ) = 
    raise NotImplemented
  in
  generate_permutations' constraints all_gaps []
;;

let complete_all_permutations (perms : square list list) (xLen : int): square list list = 
  let complete_row (lst : square list) (xLen : int) : square list = 
    raise NotImplemented
  in
  List.map (fun x -> complete_row x xLen) perms
;;

(* motherfunction *)
let compute_permutations (constraints : int list) (xLen : int) : square list list = 
  let permutations = generate_all_permutations constraints xLen in 
  complete_all_permutations permutations xLen 
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
  raise NotImplemented
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
  let rec column_validator_helper constraints current_column current_block =
    raise NotImplemented 
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
    raise NotImplemented 
  in
  validate_columns 0
;;

(* ---- DFS ---- *)
let rec dfs (depth: int) (state: grid) (grid_size: int)
            (horizontal_hints: int list list) (vertical_hints: int list list): bool =
    raise NotImplemented
;;

  



