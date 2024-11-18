
(* 
  extract_column
  - extracts a column from the grid, including the new row permutation being added 
  PARAMS:
  - (grid : square list list) - nongram grid built up so far
  - (new_row : square list) - new permutation being added to grid
  - (column_index : int) - index of column in grid
  - (row_index : int) - index of row in grid
  
  
  RETURN:
  - square list - column at column_index of grid
*)

let rec extract_column grid new_row comlumn_index row_index = 
  let base_column = List.map(fun row -> List.nth row comlumn_index) grid in
  let partial_column = if row_index = List.length grid then base_column @ [List.nth new_row comlumn_index]
  else base_column
  in 
  partial_column
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

let column_constraint_check partial_column constraint = 
  row_validity constraint partial_column

;;

(* 
  row_validity
  - checks if a given new row permutation will fit with previous rows and column constraints of all columns in grid
  PARAMS:
  - (grid : square list list) - nongram grid built up so far
  - (new_row : square list) - new permutation being added to grid
  - (row_index : int) - index of row in grid
  - (column_constraints : int list list) - list of constraints placed on columns of grid
  
  RETURN:
  - boolean - true if all proposed columns when new row has been added conform to constraints, false otherwise
*)

let row_validity grid new_row row_index column_constrains = 
  let rec validate_columns index = 
    if index >= List.length column_constrains then true
    else
      let partial_column = extract_column grid new_row index row_index 
    in
      let constraint = List.nth column_constrains index 
  in
  if column_constraint_check partial_column constraint then validate_columns (index + 1)
  else false
in
validate_columns 0

;;




