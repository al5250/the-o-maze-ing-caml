(**********************************************************************
 * CS51 Final Project, 2016
 * Maze Generation, Drawing, and Solving
 * Melissa Yu, Alex Lin
 *)

(* ====================================================================
 * Section 1: Cells
 *)

module type CELL =
  sig
    type c
    val to_string : c -> string

    (* Generate a cell *)
    val generate: int * int -> int -> c

    (* Toggle the left wall a cell *)
    val toggle_left: c -> unit

    (* Toggle the bottom wall a cell *)
    val toggle_bottom: c -> unit
  end

module SquareCell : CELL =
  struct
    type c = 
    	{ pos : int * int;
				length : int;
				mutable left : bool;
				mutable bottom : bool;
			}

    let to_string c = 
    	let (x, y) = c.pos in
    	let tmp = String.concat ", " 
    		["(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"; string_of_int c.length; 
    			string_of_bool c.left; string_of_bool c.bottom]
    	in "[" ^ tmp ^ "]"

    let generate pos length =
    	{ pos;
				length;
				left = false;
				bottom = false;
			}

    let toggle_left c =
    	c.left <- not c.left

    let toggle_bottom c =
    	c.bottom <- not c.bottom
  end

(* ====================================================================
 * Section 2: Maze
 *)

module type MAZE = 
	sig
		type cell
		type maze

    (* Generate a maze of size int *)
		val generate : int -> maze

    (* Draw a maze *)
		val draw : maze -> unit

    (* Solve a maze graphically *)
		val solve : maze -> unit
	end

module Maze (C : CELL) : (MAZE with type cell = C.c) =
	struct
		type cell = C.c
		type maze = cell list

		(* Initializes an empty maze of size n at (0, 0) *)
		let initialize (n : int) : maze = [C.generate (0, 0) n]

		(* Divides a cell into 4 cells and returns a maze containing the cells *)
		(* let divide (c : cell) : maze =
			let (x, y) = c.pos in
			let new_length = c.length / 2 in
			if new_length = 0
			then [c]
			else
				let c1 = create_cell (x, y) new_length in
				let c2 = create_cell (x, y + new_length) new_length in
				let c3 = create_cell (x + new_length, y + new_length) new_length in 
				let c4 = create_cell (x + new_length, y) new_length in 
				match Random.int 4 with
					| 0 -> c2.bottom <- true; [c1; c2; c3; c4]
					| 1 -> c3.left <- true; [c1; c2; c3; c4]
					| 2 -> c3.bottom <- true; [c1; c2; c3; c4]
					| 3 -> c4.left <- true; [c1; c2; c3; c4] *)

		(* Helper function to generate a maze *)
		(* let rec generate' (m : maze) : maze = 
			let new_m = List.fold_left (fun a e -> (divide e) @ a) [] m in
			if new_m = m
			then m
			else generate' new_m  *)

		let generate n = initialize n

		let draw m = ()

		let solve m = ()
	end

module SquareMaze = (Maze(SquareCell) :
                         MAZE with type cell = SquareCell.c)




