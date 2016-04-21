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

    (* Get cell values *)
    val get_pos: c -> int * int
    val get_len: c -> int
    val get_left: c -> bool
    val get_bottom: c -> bool
  end

module SquareCell =
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
    	in "{" ^ tmp ^ "}"

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

    let get_pos c = c.pos
    let get_len c = c.length
    let get_left c = c.left
    let get_bottom c = c.bottom
  end

(* ====================================================================
 * Section 2: Maze
 *)

module type MAZE = 
	sig
		type cell
		type maze
		val to_string : maze -> string

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

		let to_string m = 
    	List.fold_left (fun a e -> a ^ (C.to_string e)) "" m

		(* Initializes an empty maze of size n at (0, 0) *)
		let initialize (n : int) : maze = [C.generate (0, 0) n]

		(* Helper function for divide *)
		let modify (cell1 : cell) (cell2 : cell) (left_space : bool) : unit =
			match Random.int 2 with
			| 0 -> 
				if left_space then C.toggle_left cell1
				else C.toggle_bottom cell1
			| 1 -> 
				if left_space then C.toggle_left cell2
				else C.toggle_bottom cell2
			
		(* Divides a cell into 4 cells and returns a maze containing the cells *)
		let divide (c : cell) : maze =
			let (x, y) = C.get_pos c in
			let dl = (C.get_len c) / 2 in
			if dl = 0 then [c]
			else
				let c1 = C.generate (x, y) dl in
				let c2 = C.generate (x, y + dl) dl in
				let c3 = C.generate (x + dl, y + dl) dl in 
				let c4 = C.generate (x + dl, y) dl in 
				if C.get_left c then modify c1 c2 true;
				if C.get_bottom c then modify c1 c4 false;
				match Random.int 4 with
				| 0 -> C.toggle_left c3; C.toggle_bottom c3; C.toggle_left c4; [c1; c2; c3; c4]
				| 1 -> C.toggle_bottom c2; C.toggle_bottom c3; C.toggle_left c4; [c1; c2; c3; c4]
				| 2 -> C.toggle_bottom c2; C.toggle_left c3; C.toggle_left c4; [c1; c2; c3; c4]
				| 3 -> C.toggle_bottom c2; C.toggle_left c3; C.toggle_bottom c3; [c1; c2; c3; c4]

		let generate n = 
			let rec generate' (m : maze) : maze = 
				let hd::tl = m in
				if C.get_len hd = 1 then m
				else generate' (List.fold_left (fun a e -> a @ (divide e)) [] m)
			in
			generate' (initialize n)

		let draw m = ()

		let solve m = ()
	end

(* Make a square maze with square cells *)
module SquareMaze = (Maze(SquareCell) :
                         MAZE with type cell = SquareCell.c)




