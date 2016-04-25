(**********************************************************************
 * CS51 Final Project, Spring 2016
 * Maze Generation, Drawing, and Solving
 * Melissa Yu, Alex Lin
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
        ["(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"; 
        string_of_int c.length; string_of_bool c.left; string_of_bool c.bottom]
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
        match (Random.int 4) with
        | 0 -> open_left c3; open_bottom c3; open_left c4; [c1; c2; c3; c4]
        | 1 -> open_bottom c2; open_bottom c3; open_left c4; [c1; c2; c3; c4]
        | 2 -> open_bottom c2; open_left c3; open_left c4; [c1; c2; c3; c4]
        | 3 -> open_bottom c2; open_left c3; open_bottom c3; [c1; c2; c3; c4]
        | _ -> failwith "random number generator error"

    let get_pos c = c.pos

    let get_dim c = (c.length, c.length)

    let get_left c = c.left

    let get_bottom c = c.bottom

  end
