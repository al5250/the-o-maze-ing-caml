(**********************************************************************
 * CS51 Final Project, Spring 2016
 * Maze Generation, Drawing, and Solving
 * Melissa Yu, Alex Lin
 *)

(* ====================================================================
 * Section 1: Cells
 *)

open Graphics

#load "graphics.cma";;

module type CELL =
  sig
    type c 
    val to_string : c -> string

    (* Generate a cell *)
    val generate: int * int -> int -> c

    (* Open the left wall of a cell *)
    val open_left: c -> unit

    (* Open the bottom wall of a cell *)
    val open_bottom: c -> unit
        
    (* Divide a cell into 4 cells and return a list of the new cells *)
    val divide: c -> c list

    (* Get cell values *)
    val get_pos: c -> int * int
    val get_dim: c -> int * int
    val get_left: c -> bool
    val get_bottom: c -> bool
  end

module SquareCell =
  struct
    (* Square cell: 
     * Position: Coordinates of bottom left corner of cell
     * Length: Length of one side of a cell
     * Left: Boolean indicating whether the left wall has a hole;
     *       True if wall has hole, false if wall is solid
     * Bottom: Boolean for bottom wall *)
    type c = {
      pos             : int * int;
      length          : int;
      mutable left    : bool;
      mutable bottom  : bool;
      }

    let to_string c = 
      let (x, y) = c.pos in
      let tmp = String.concat ", " 
          ["(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"; string_of_int c.length; 
              string_of_bool c.left; string_of_bool c.bottom]
      in "<" ^ tmp ^ "> "

    let generate pos length =
      { pos;
        length;
        left = false;
        bottom = false;
      }

    let open_left c =
      c.left <- not c.left

    let open_bottom c =
      c.bottom <- not c.bottom

    (* Helper function for divide *)
    let modify (c1 : c) (c2 : c) (left_space : bool) : unit =
      match Random.int 2 with
      | 0 -> 
        if left_space then open_left c1
        else open_bottom c1
      | 1 -> 
        if left_space then open_left c2
        else open_bottom c2

    let divide (c : c) : c list =
      let (x, y) = c.pos in
      let dl = c.length / 2 in
      if dl = 0 then [c]
      else
        let c1 = generate (x, y) dl in
        let c2 = generate (x, y + dl) dl in
        let c3 = generate (x + dl, y + dl) dl in 
        let c4 = generate (x + dl, y) dl in 
        if c.left then modify c1 c2 true;
        if c.bottom then modify c1 c4 false;
        match Random.int 4 with
        | 0 -> open_left c3; open_bottom c3; open_left c4; [c1; c2; c3; c4]
        | 1 -> open_bottom c2; open_bottom c3; open_left c4; [c1; c2; c3; c4]
        | 2 -> open_bottom c2; open_left c3; open_left c4; [c1; c2; c3; c4]
        | 3 -> open_bottom c2; open_left c3; open_bottom c3; [c1; c2; c3; c4]

    let get_pos c = c.pos
    let get_dim c = (c.length, c.length)
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

  (* Generate a maze of size n, where n is a power of 2 *)
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

    (* ##### GENERATE ##### *)

    (* Initializes an empty maze of size n at (0, 0) *)
    let initialize (n : int) : maze = [C.generate (0, 0) n]

    let generate n = 
      let rec generate' (m : maze) : maze = 
        match m with
        | [] -> failwith "Invalid maze"
        | hd::tl ->
          let dims = C.get_dim hd in
          if fst dims = 1 || snd dims = 1 then m
          else generate' (List.fold_left (fun a e -> List.append (C.divide e) a) [] m)
      in
      generate' (initialize n)

    (* ##### DRAW ##### *)

    (* sets up the screen *)
    (* screen is 700 by 700; actual maze is 512 by 512 *)
    let display_screen () : unit = 
      open_graph " 700x700"
      (* display title *)
      (* draw border *)

    (* draw a single cell *)
    let draw_cell c : unit = ()

    (* draw the maze based on cell list *)
    let draw_maze m : unit = () 

    (* randomly remove 2 walls on border for start and end points *)
    let finish_draw () : unit = ()

    (* do the entire process *)
    let draw m = 
      display_screen ();
      draw_maze m;
      finish_draw ()

    (* ##### SOLVE ##### *)
      
    let solve m = ()
    
  end

(* Make a square maze with square cells *)
module SquareMaze = (Maze(SquareCell) : MAZE with type cell = SquareCell.c)




