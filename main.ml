(* Exception in case we couldn't find a solution. *)
exception NotFound

(* Type definition *)
type grid = int list list (* Grid is represented as a matrix. *)
type 'a tree = Empty | Node of ('a tree) list * 'a
type 'a pair = 'a * 'a
type hints = (int list list) pair (* horizontal and vertical *)

(* Returns the first value of the pair *)
let first p: 'a pair = 
    match p with
    | (x, y) -> x

(* Returns the first value of the pair *)
let second p: 'a pair = 
    match p with
    | (x, y) -> y

let list_sum l = 
    List.fold_left (+) 0 l

(*  *)
let rec generate_candidates (hint: int list) grid_length = 
    let rec insert_spaces remaining_blocks remaining_spaces acc = 
        match remaining_blocks with
        | [] -> 

(*
(* depth is 0-indexed. *)
let rec search (input: hints) (current: grid tree) (depth: int) = 
    match current with
    | Empty -> raise not_found
    | Node(tree_list, node) ->
            let vertical_hint = List.nth (second hints) depth in
            let horizontal_hint = first hints
            let hint_size = List.length (List.nth (second hints) depth) in
            let hint_sum = list_sum 
*)
let main () = 
    let test_inputs: hints list = 
        [([[3];[1];[3];[2];[4]], [[1];[1;1];[3;1];[3];[3]])] in
    Printf.printf "Hello, world!\n"

let () = main ()
