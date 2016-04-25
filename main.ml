(**********************************************************************
 * CS51 Final Project, Spring 2016
 * Maze Generation, Drawing, and Solving
 * Melissa Yu, Alex Lin
 *)

open Cell
open Maze

(* makes a square maze with square cells *)
module SquareMaze = (Maze(SquareCell) : MAZE with type cell = SquareCell.c)

let usage () = print_string "invalid input, please try again\n"

(* asks user if he/she wants to randomly generate a new maze *)
let rec new_maze () =
  let () = print_string "new maze? (y/n): " in
  match read_line () with
  | "y" -> enter_diff ()
	| "n" -> print_newline ()
	| _ -> usage (); new_maze ()

(* asks user for difficulty level *)
and enter_diff () =
	let () = print_string "enter a difficulty (between 1 and 7) to generate maze: " in
  try 
  	let level = read_int () in 
  	if (level < 0) || (level > 7) then (usage (); enter_diff ())
  	else
  		let maze = SquareMaze.generate (int_of_float (2. ** (float_of_int level))) in
  		show_solution maze
  with Failure _ -> usage (); enter_diff ()

(* asks user if he/she wants to see the solution to the maze *)
and show_solution maze =
	let () = print_string "see solution? (y/n): " in
	match read_line () with
	| "y" -> (SquareMaze.solve maze; new_maze ())
	| "n" -> new_maze ()
	| _ -> usage (); show_solution maze;;

new_maze () ;;
