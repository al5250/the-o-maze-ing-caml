module type MAZE = 
	sig
		type cell
		type maze
		val generate : int -> maze
		(* 		val draw : maze -> unit
		val solve : maze -> unit *)
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
				match Random.int 4 with
					| 0 -> c2.bottom <- true; [c1; c2; c3; c4]
					| 1 -> c3.left <- true; [c1; c2; c3; c4]
					| 2 -> c3.bottom <- true; [c1; c2; c3; c4]
					| 3 -> c4.left <- true; [c1; c2; c3; c4]

		let rec generate' (m : maze) : maze = 
			let new_m = List.fold_left (fun a e -> (divide e) @ a) [] m in
			if new_m = m
			then m
			else generate' new_m 

		let generate (n : int) = 
			generate' (initialize n)

	end

