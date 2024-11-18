
let rec extract_column grid new_row comlumn_index row_index = 
  let base_column = List.map(fun row -> List.nth row comlumn_index) grid in
  let partial_column = if row_index = List.length grid then base_column @ [List.nth new_row comlumn_index]
  else base_column
  in 
  partial_column
;;

let column_constraint_check partial_column constraint = 
  row_validity constraint partial_column

;;

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




