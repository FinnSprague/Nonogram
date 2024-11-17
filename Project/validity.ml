(* 
  validity
  - checks if a prposed row is valid given a set of constraints 
  PARAMS:
  - (constraints : int list) - numerical constraints on row
  - (row: square list) - a row in nonogram square
  
  RETURN:
  - boolean - returns true if valid row, false otherwise
*)

let validity constraints row = 
  (* 
  extractor
  - converts list of square objects into list of integers representing extracted blocks  
  PARAMS:
  - (row: square list) - a row in nonogram square
  - (current_block: int) - number of boxes in curent block
  - (acc: int list) - list of blocks
  
  RETURN:
  - int list - returns list of boxes
  *) 
  let rec extractor row current_block acc = match row with
  |[] -> if current_block > 0 then acc @ [current_block] else acc
  |BOX :: tl -> extractor tl (current_block + 1) acc
  |EMP :: tl | BLANK :: tl -> let updated_acc = 
                              if current_block > 0 then acc @ [current_block] else acc in 
                              extractor tl 0 updated_acc
  in
  let blocks = extractor row 0 [] in
  (* 
  constraint_handler
  - checks if provided list of boxes satisfies given constraints  
  PARAMS:
  - (constraints: int list) - a row in nonogram square
  - (blocks: int list) - list of blocks
  
  RETURN:
  - boolean - returns true if blocks satsitisfes contraints, false otherwise
  *) 
  let rec constraint_handler constraints blocks = match constraints, blocks with
  |[], [] -> true
  |[], _ -> List.for_all (fun block -> block = 0) blocks
  |_, [] -> List.for_all (fun constraint -> constraint = 0) constraints
  |constraint :: constraint_tail, block :: block_tail -> if constraint <= block then 
    constraint_handler constraint_tail block_tail else false
  in 
  constraint_handler constraints blocks

