
type square = BOX | EMP | BLANK ;;

(* 
  repeat_square
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

let rec len lst = 
  match lst with 
  | [] -> 0
  | h :: t -> 1 + len t 
;; 

(* 
  insert_empty
  PARAMS:
  - (constraints : int list) - numerical contraints on row
  - (gap : int) - length of gap inserted into create_row

  RETURN: 
  - A row of combined SQUARE and EMPTY types. Row length might exceed or subceed total row length. 
*)

let rec create_row (constraints : int list) (gap : int): (square list) =
  Format.print_string("creating row... \n") ; 
  match constraints with 
  | [] -> []
  | h :: t -> (repeat_square BOX h) @ (repeat_square EMP gap) @ create_row t gap 
;;

(* 
  verify_row
  PARAMS:
  - (row : square list) - list of square types 
  - (xLen : int) - total length of row

  RETURN: 
  - 1 if the row is less than the total length, 0 if it equals, -1 if the row is too big. 
*)
let verify_row (row : square list) (xLen : int) : int = 
  if len row < xLen then 1 else 
    if len row = xLen then 0 else -1
  ;;
;;

(* 
  complete_row
  PARAMS:
  - (row : square list) - row with squares
  - (xLen : int) - total length of row

  RETURN: 
  - completes the rows that are too short with EMPTY squares. 
*)
let complete_row (row : square list) (xLen : int) : square list = 
  let res = verify_row row xLen in 
  if res = 0 then row 
  else if res = -1 then [] 
  else 
    let remainingSquares = xLen - (len row) in 
    row @ repeat_square EMP remainingSquares
  ;; 
;;


(*
  generate_permutation
  - generates a single permutation according to the gap between BOX's

  PARAMS:
  - (constraints : int) - constraints on row
  - (xLen : int) - length of row
  - (gap : int)

  RETURN
  - square list - a single permutation of a row 
*)
let generate_permutation (constraints : int list ) (xLen : int ) (gap : int) : (square list ) = 
  let perm = create_row constraints gap in
  let perm_verify = verify_row perm xLen in
  if perm_verify = 1 then complete_row perm xLen 
  else if perm_verify = -1 then [] 
  else perm 
;;

(* gen_permutation_list 
    - generates a grid of rows ALL according to a single gap value
*)

let gen_permutation_list (constraints : int list list) (xLen : int) : (square list list ) = 
  List.map (fun x -> generate_permutation x xLen 1) constraints ;;


(* PRINTING METHODS *)

let format_square (sq : square) : string = 
  if sq = BOX then "BOX" else if sq = EMP then "EMP" else if sq = BLANK then "BLANK" else "" 
;;

let rec print_row (row : square list) : unit = 
  Format.print_string(" | ") ; 
  match row with 
  | [] -> Format.print_string("\n")
  | h :: t -> Format.print_string(format_square h) ; print_row t 
;;

let rec print_grid (grid : square list list) : unit = 
  Format.print_string("\n") ; 
  match grid with 
  | [] -> Format.print_string(" ")
  | [[]] -> Format.print_string(" ") 
  | h :: t -> print_row h ; print_grid t 
;;

let sample_grid = [
  [1;2;3] ;
  [2;3] ;
  [4; 1] ;
  [3; 4]
] ;;

print_grid (gen_permutation_list sample_grid 10) ;; 
