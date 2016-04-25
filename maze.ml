(***********************************************************
 * CS51 Final Project, Spring 2016
 * Maze Generation, Drawing, and Solving
 * Melissa Yu, Alex Lin
 *)

open Graphics
open Cell

(**** Constants ****)
let window_size = 612
let margin = 50
let maze_size = window_size - 2 * margin

module type MAZE = 
  sig
    (* representation of one cell in maze *)
    type cell

    (* representation of maze composed of cells *)
    type maze

    (* representation of stack of coordinates for solving *)
    type positions

    (* converts maze to string *)
    val maze_to_string : maze -> string

    (* converts list of positions to string *)
    val positions_to_string : positions -> string

    (* generate a maze of size n, where n is a power of 2 *)
    val generate : int -> maze

    (* solve a maze *)
    val solve : maze -> positions

  end

module Maze (C : CELL) : (MAZE with type cell = C.c) =
  struct
    type cell = C.c
    type maze = cell list
    type positions = (int * int) list

    (* cell representation for solving *)
    type array_cell = {
      top     : bool;
      bottom  : bool;
      left    : bool;
      right   : bool;
    }

    (* maze representation for solving *)
    type array_maze = array_cell array array

    (***********************)
    (******* Drawing *******)
    (***********************)

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
      let (screen_x, screen_y) = 
        (scale * x + margin, scale * y + margin) in
      moveto screen_x screen_y;
      if not (C.get_left c) then lineto screen_x (screen_y + scale);
      moveto screen_x screen_y;
      if not (C.get_bottom c) then lineto (screen_x + scale) screen_y

    (* draw the maze based on cell list *)
    let draw_maze (m : maze) : unit = 
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

    (* draw blank maze *)
    let draw m = 
      close_graph ();
      display_screen ();
      draw_maze m

    (* draw maze solution given dimension of matrix *)
    let draw_solution (soln : positions) (n : int) : unit = 
      let scale = maze_size / n in
      set_line_width 2;
      set_color red;
      moveto (margin + (n-1) * scale  + scale/2) (margin + scale * n);  
      List.iter (fun (x,y) -> 
        lineto (margin + x * scale + scale/2) 
          (margin + y * scale + scale/2)) soln;
      lineto (margin + scale/2) margin

    (***********************)
    (*** Maze Generation ***)
    (***********************)

    let maze_to_string m = 
      List.fold_left (fun a e -> a ^ (C.to_string e)) "" m

    (* initializes an empty maze of size n at (0, 0) *)
    let initialize (n : int) : maze = [C.generate (0, 0) n]

    let generate n = 
      let rec generate' (m : maze) : maze = 
        match m with
        | [] -> failwith "Invalid maze"
        | hd::_ ->
          let dims = C.get_dim hd in
          if fst dims = 1 || snd dims = 1 then m
          else generate' (List.fold_left (fun a e -> 
            List.append (C.divide e) a) [] m)
      in
      let new_maze = generate' (initialize n) in
      draw new_maze;
      new_maze

    (***********************)
    (**** Maze Solving *****)
    (***********************)
    
    let positions_to_string (path : positions) : string =
      List.fold_left (fun a e -> "(" ^ (string_of_int (fst e)) ^ ","
        ^ (string_of_int (snd e)) ^ "), " ^ a) "" path

    (* populate matrix with cell data *)
    let to_matrix (m : maze) : array_maze = 
      let n = int_of_float (sqrt (float_of_int (List.length m))) in
      let maze_array = Array.make_matrix n n 
        {top=false; bottom=false; left=false; right=false} in
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

    (* true if pos2 is accessible from pos1 in the maze *)
    let is_neighbor (m : array_maze) 
      (pos1 : int * int) (pos2 : int * int) : bool =
        let (x1, y1) = pos1 in
        let (x2, y2) = pos2 in
        match (x1 - x2, y1 - y2) with
        | (0,1) -> m.(x1).(y1).bottom 
        | (1,0) -> m.(x1).(y1).left 
        | (0,-1) -> m.(x1).(y1).top 
        | (-1,0) -> m.(x1).(y1).right 
        | _ -> false

    (* finds all adjacent and accessible cells to the current cell *)
    let get_neighbors (m : array_maze) ((x, y) : int * int) : positions =
      let n = Array.length m in 
      let neighbors = ref [] in neighbors := [];
      if (y < n-1) && (is_neighbor m (x, y) (x, y+1)) then 
        neighbors := (x, y+1) :: !neighbors; 
      if (y > 0) && (is_neighbor m (x, y) (x, y-1)) then 
        neighbors := (x, y-1) :: !neighbors;
      if (x < n-1) && (is_neighbor m (x, y) (x+1, y)) then 
        neighbors := (x+1, y) :: !neighbors;
      if (x > 0) && (is_neighbor m (x, y) (x-1, y)) then 
        neighbors := (x-1, y) :: !neighbors;
      !neighbors

    (* backtracks along the current path until a new path is found *)
    let rec backtrack (m : array_maze) (frontier : positions ref) 
      (path : positions ref) : unit =
        match !path with
        | [] -> raise (Invalid_argument "Backtrack: empty path")
        | curr_cell :: tl ->
          match !frontier with
          | [] -> raise (Invalid_argument "Backtrack: empty frontier")
          | next_cell :: _ -> 
            if is_neighbor m next_cell curr_cell then ()
            else 
              (path := tl; 
              backtrack m frontier path)

    (* keeps exploring/extending the path forward until we hit a dead end *)
    let rec explore (m : array_maze) (frontier : positions ref) 
      (path : positions ref) : unit = 
        let n = Array.length m in 
        match !frontier with
        | [] -> raise (Invalid_argument "Explore: empty frontier")
        | curr_cell :: t -> 
          if curr_cell = (n-1, n-1) then (path := curr_cell :: !path; ())
          else begin
            frontier := t;
            let filter_helper path e = 
              match !path with
              | [] -> true 
              | hd :: _ -> e <> hd
            in
            match List.filter (filter_helper path) (get_neighbors m curr_cell) with
            | [] -> 
              (backtrack m frontier path; 
              explore m frontier path)
            | nb -> 
              (List.iter (fun e -> frontier := e :: !frontier) nb;
              path := curr_cell :: !path; 
              explore m frontier path)
          end

    (* computes the path from the start to the end of a maze *)
    let find_path (m : array_maze) : positions = 
      let frontier = ref [(0, 0)] in
      let path = ref [] in
      explore m frontier path;
      !path

    (* finds the solution to an already rendered maze and draws it *)
    let solve m = 
      let matrix_m = to_matrix m in
      let solution = find_path matrix_m in
      draw_solution solution (Array.length matrix_m);
      solution
    
  end
  