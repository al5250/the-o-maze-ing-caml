(**********************************************************************
 * CS51 Final Project, Spring 2016
 * Maze Generation, Drawing, and Solving
 * Melissa Yu, Alex Lin
 *)

open Graphics;;
open Array;;

#load "graphics.cma";;

(* ##### GLOBAL VARIABLES ##### *)
let window_size = 612
let margin = 50
let maze_size = window_size - 2 * margin


(* ====================================================================
 * Section 1: Cells
 *)

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

    (* returns coordinates (x,y) of bottom left corner of cell *)
    val get_pos: c -> int * int

    (* returns tuple (width,height) of cell *)
    val get_dim: c -> int * int

    (* returns boolean indicating if left wall has hole *)
    val get_left: c -> bool

    (* returns boolean indicating if bottom wall has hole *)
    val get_bottom: c -> bool
  end

module SquareCell : CELL =
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
      c.left <- true

    let open_bottom c =
      c.bottom <- true

    (* applies f to one of two cells at random *)
    let modify (c1 : c) (c2 : c) (f : c -> unit) : unit =
      if Random.int 2 = 0 then f c1 else f c2

    let divide (c : c) : c list =
      let (x, y) = c.pos in
      let dl = c.length / 2 in
      if dl = 0 then [c]
      else
        let c1 = generate (x, y) dl in
        let c2 = generate (x, y + dl) dl in
        let c3 = generate (x + dl, y + dl) dl in 
        let c4 = generate (x + dl, y) dl in 
        if c.left then modify c1 c2 open_left;
        if c.bottom then modify c1 c4 open_bottom;
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
    type array_cell
    type array_maze
    val to_string : maze -> string

    (* Generate a maze of size n, where n is a power of 2 *)
    val generate : int -> maze

    (* Solve a maze graphically *)
    val solve : maze -> unit

    val to_matrix : maze -> array_maze

    val print_cell : array_cell -> unit

    val get_cell : array_maze -> int -> int -> array_cell
  end

module Maze (C : CELL) : (MAZE with type cell = C.c) =
  struct
    type cell = C.c
    type maze = cell list

    (* cell representation for solving *)
    type array_cell = {
      top     : bool;
      bottom  : bool;
      left    : bool;
      right   : bool;
    }

    (* maze representation for solving *)
    type array_maze = array_cell array array

    let to_string m = 
      List.fold_left (fun a e -> a ^ (C.to_string e)) "" m

    (* ##### DRAW ##### *)

    (* sets up the screen *)
    let display_screen () : unit = 
      open_graph (" " ^ string_of_int window_size ^ "x" 
        ^ string_of_int window_size);
      set_window_title "The O-Maze-ing Caml";
      draw_rect margin margin maze_size maze_size

    (* draw a single cell given length of one side of maze *)
    let draw_cell (n : int) (c : cell) : unit = 
      let (x,y) = C.get_pos c in
      let scale = maze_size / n in
      let (screen_x, screen_y) = (scale * x + margin, scale * y + margin) in
      moveto screen_x screen_y;
      if not (C.get_left c) then lineto screen_x (screen_y + scale);
      moveto screen_x screen_y;
      if not (C.get_bottom c) then lineto (screen_x + scale) screen_y

    (* draw the maze based on cell list *)
    let draw_maze m : unit = 
      let n = int_of_float (sqrt (float_of_int (List.length m))) in
      let scale = maze_size / n in
      List.iter (draw_cell n) m;

      (* finish drawing - color and label start and end points *)
      set_color red;
      set_line_width 4;
      moveto margin margin;
      lineto (margin + scale) margin;
      moveto (margin + maze_size - scale) (margin + maze_size) ;
      lineto (margin + maze_size) (margin + maze_size);
      moveto margin (margin - 15);
      draw_string "Start Here";
      moveto (margin + maze_size - scale) (margin + maze_size + 4);
      draw_string "End Here"

    (* do the entire process *)
    let draw m = 
      close_graph ();
      display_screen ();
      draw_maze m

    (* ##### GENERATE ##### *)

    (* initializes an empty maze of size n at (0, 0) *)
    let initialize (n : int) : maze = [C.generate (0, 0) n]

    let generate n = 
      let rec generate' (m : maze) : maze = 
        match m with
        | [] -> failwith "Invalid maze"
        | hd::tl ->
          let dims = C.get_dim hd in
          if fst dims = 1 || snd dims = 1 then m
          else generate' (List.fold_left (fun a e -> 
            List.append (C.divide e) a) [] m)
      in
      let new_maze = generate' (initialize n) in
      draw new_maze;
      new_maze

    (* ##### SOLVE ##### *)
    
    (* populate matrix with cell data *)
    let to_matrix (m : maze) : array_maze = 
      let n = int_of_float (sqrt (float_of_int (List.length m))) in
      let maze_array = make_matrix n n {top=false; bottom=false; left=false; right=false} in
      let to_matrix' (m : array_maze) (c : cell) : unit =
        let (x, y) = C.get_pos c in
        let left' = C.get_left c in
        let bottom' = C.get_bottom c in
        m.(x).(y) <- {m.(x).(y) with left=left'; bottom=bottom'};
        if y > 0 then m.(x).(y-1) <- {m.(x).(y-1) with top=bottom'};
        if x > 0 then m.(x-1).(y) <- {m.(x-1).(y) with right=left'};
      in
      List.iter (to_matrix' maze_array) m;
      maze_array

      (* try to go up, right, down, left *)
      (* if move is beyond maze bounds or blocked by a wall, go back and try next direction *)
      (* if move is allowed, move to new cell, paint cell, and call try_move again *)
      (* after all moves have been exhausted, trace back to last cell *)


    let find_path (m : array_maze) : (int * int) list = 
      let n = Array.length m in 
      let frontier = ref [(0, 0)] in
      let path = ref [(0, 0)] in
      let is_neighbor (pos1 : int * int) (pos2 : int * int) : bool = 
        let (x1, y1) = pos1 in
        let (x2, y2) = pos2 in
        match (x1 - x2, y1 - y2) with
        | (0,1) -> m.(x1).(y1).bottom 
        | (1,0) -> m.(x1).(y1).left 
        | (0,-1) -> m.(x1).(y1).top 
        | (-1,0) -> m.(x1).(y1).right 
        | _ -> false
      in
      let get_neighbors ((x, y) : int * int) : (int * int) list = 
        let neighbors = ref [] in
        if (y < n-1) && (is_neighbor (x, y) (x, y+1)) then neighbors := (x, y+1)::!neighbors; 
        if (y > 0) && (is_neighbor (x, y) (x, y-1)) then neighbors := (x, y-1)::!neighbors;
        if (x < n-1) && (is_neighbor (x, y) (x+1, y)) then neighbors := (x+1, y)::!neighbors;
        if (x > 0) && (is_neighbor (x, y) (x-1, y)) then neighbors := (x-1, y)::!neighbors;
        !neighbors
      in  
      let rec backtrack () : unit =
        let h::t = !path in 
        if is_neighbor (List.hd !frontier) h 
        then ()
        else 
        path := t;
        backtrack ()
      in 
      let rec explore () : unit = 
        let h::t = !frontier in
        if h = (n-1, n-1) then () 
        else
        frontier := t;
        match get_neighbors h with
        | [] -> backtrack (); explore ()
        | nb -> 
          List.iter (fun e -> 
            if e <> List.hd !path then frontier := e::!frontier) nb;
          h::!path; 
          explore ()
      in
      explore ();
      !path

    let get_cell (m : array_maze) (n1 : int) (n2 : int) : array_cell =
      m.(n1).(n2)

    let print_cell (c : array_cell) : unit =
      print_string ("(" ^ string_of_bool c.top ^ "," ^ string_of_bool c.right ^ ","
        ^ string_of_bool c.bottom ^ "," ^ string_of_bool c.left ^ "), ")

    let print_shit (melly : (int * int) list) : unit =
      print_string (List.fold_left (fun a e -> "(" ^ (string_of_int (fst e)) ^ "," ^ (string_of_int (snd e)) ^ "), ") "" melly) 

    let solve m = 
      close_graph ();
      draw m;
      print_shit (find_path (to_matrix m))
    
  end

(* Make a square maze with square cells *)
module SquareMaze = (Maze(SquareCell) : MAZE with type cell = SquareCell.c)




