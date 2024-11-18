
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

let column_validator partial_column constraint = 
  let rec column_validator_helper constraints current_column current_block = match constraints, current_column with
  |[], [] -> current_block = 0
  |[], EMP :: tl -> column_validator_helper [] tl current_block
  |[], BOX :: _ -> false
  |h :: tlc, BOX :: tl -> column_validator_helper (if current_block + 1 = h then tlc else  constraints) tl (if current_block + 1 = h then 0 else  current_block + 1)
  |h :: _, EMP:: tl -> if current_block > 0 then false else column_validator_helper constraints tl 0
  |h :: _, [] when current_block > 0 -> current_block = h
  |_, [] -> true
in
column_validator_helper partial_column constraint 0

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

let row_validity grid new_row column_constrains = 
  let rec validate_columns index = 
    if index >= List.length column_constrains then true
    else
      let partial_column = extract_column grid new_row index in
      let constraint = List.nth column_constrains index in
      if column_validator partial_column constraint then validate_columns (index + 1)
      else false
in
validate_columns 0

;;




