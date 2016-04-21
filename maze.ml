module type MAZE = 
	sig
		type cell
		type maze
		val generate : int -> maze
		(* 		val draw : maze -> unit
		val solve : maze -> unit *)
		val maze_to_string : maze -> string
	end

module Maze : MAZE = 
	struct
		type cell = {
			pos : int*int;
			length : int;
			mutable left : bool;
			mutable bottom : bool;
		}

		type maze = cell list

		let create_cell (p : int * int) (l : int) : cell =
			{
				pos = p;
				length = l;
				left = false;
				bottom = false;
			}

		let initialize (n : int) : maze = 
			[create_cell (0, 0) n]

		let divide (c : cell) : maze =
			let (x, y) = c.pos in
			let new_length = c.length / 2 in
			if new_length = 0
			then [c]
			else
				let c1 = create_cell (x, y) new_length in
				let c2 = create_cell (x, y + new_length) new_length in
				let c3 = create_cell (x + new_length, y + new_length) new_length in 
				let c4 = create_cell (x + new_length, y) new_length in 
				let modify (cell1 : cell) (cell2 : cell) (left_space : bool) : unit =
					match Random.int 2 with
					| 0 -> 
						if left_space
						then cell1.left <- true
						else cell1.bottom <- true
					| 1 -> 
						if left_space
						then cell2.left <- true
						else cell2.bottom <- true
				in
				if c.left
				then modify c1 c2 true;
				if c.bottom
				then modify c1 c4 false;
				match Random.int 4 with
					| 0 -> c2.bottom <- false; c3.left <- true; c3.bottom <- true; c4.left <- true; [c1; c2; c3; c4]
					| 1 -> c2.bottom <- true; c3.left <- false; c3.bottom <- true; c4.left <- true; [c1; c2; c3; c4]
					| 2 -> c2.bottom <- true; c3.left <- true; c3.bottom <- false; c4.left <- true; [c1; c2; c3; c4]
					| 3 -> c2.bottom <- true; c3.left <- true; c3.bottom <- true; c4.left <- false; [c1; c2; c3; c4]

		let rec generate' (m : maze) : maze = 
			if (List.hd m).length = 1
			then m
			else generate' (List.fold_left (fun a e -> a @ (divide e)) [] m)

		let generate (n : int) = 
			generate' (initialize n)

		let cell_to_string (c : cell) : string = 
    	let (x, y) = c.pos in
    	let tmp = String.concat "," [string_of_int x; string_of_int y; string_of_int c.length; 
    			string_of_bool c.left; string_of_bool c.bottom]
    	in "[" ^ tmp ^ "]"

    let maze_to_string (m : maze) : string = 
    	List.fold_left (fun a e -> a ^ (cell_to_string e)) "" m

	end

