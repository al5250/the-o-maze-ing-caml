module type MAZE = 
sig
		type cell
		type maze
		val generate : int -> maze
(* 		val draw : maze -> unit
		val solve : maze -> unit *)
end ;;

module Maze : MAZE = 
struct

	type cell = {
		pos : int;
		length : int;
		left : bool;
		bottom : bool;
		neighbors : int ref * int ref * int ref * int ref (* (top, right, bottom, left) *)
	}

	type maze = cell list

	let initialize (n : int) : maze = 
	[] 

end

