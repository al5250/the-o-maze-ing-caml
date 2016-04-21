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
			left : bool;
			bottom : bool;
			(* (top, right, bottom, left) *)
			neighbors : (cell ref * cell ref * cell ref * cell ref) 
		}

		type maze = cell list


		let initialize (n : int) : maze = 
			let c = {
				pos = (0,0);
				length = n;
				left = false;
				bottom = false;
				neighbors = None
			} 
			in [c] 


	end

