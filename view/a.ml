(* open Printf *)
open Scanf
open ExtLib

module MyGraphics = struct
  type t = {
    mutable x1 : float;
    mutable y1 : float;
    mutable x2 : float;
    mutable y2 : float;
  }

  let to_int_position self x y =
    let nx = int_of_float ((x -. self.x1) /. (self.x2 -. self.x1) *. float (Graphics.size_x ())) in
    let ny = int_of_float ((y -. self.y1) /. (self.y2 -. self.y1) *. float (Graphics.size_y ())) in
    (nx, ny)

  let to_float_position self nx ny =
    let x = self.x1 +. float nx *. (self.x2 -. self.x1) /. float (Graphics.size_x ()) in
    let y = self.y1 +. float ny *. (self.y2 -. self.y1) /. float (Graphics.size_y ()) in
    (x, y)

  let init ?(x1 = 0.0) ?(y1 = 0.0) ?(x2 = 1.0) ?(y2 = 1.0) geometry =
    Graphics.open_graph (" " ^ geometry);
    { x1; x2; y1; y2 }

  let set_x1y1x2y2 self ~x1 ~y1 ~x2 ~y2 =
    self.x1 <- x1;
    self.y1 <- y1;
    self.x2 <- x2;
    self.y2 <- y2

  let moveto self x y =
    let nx, ny = to_int_position self x y in
    Graphics.moveto nx ny

  let lineto self x y =
    let nx, ny = to_int_position self x y in
    Graphics.lineto nx ny

  let draw_poly self ary =
    let ary = Array.map (fun (x, y) -> to_int_position self x y) ary in
    Graphics.draw_poly ary

  let draw_poly_line self ary =
    let ary = Array.map (fun (x, y) -> to_int_position self x y) ary in
    Graphics.draw_poly_line ary
end

let get_minmax ary =
  let xmin, xmax, ymin, ymax =
    Array.fold_left
      (fun (xmin, xmax, ymin, ymax) (x, y) ->
         let xmin = if x < xmin then x else xmin in
         let xmax = if x > xmax then x else xmax in
         let ymin = if y < ymin then y else ymin in
         let ymax = if y > ymax then y else ymax in
         (xmin, xmax, ymin, ymax))
      (max_float, -.max_float, max_float, -.max_float)
      ary
  in
  (xmin, xmax, ymin, ymax)

let () =
  let g = MyGraphics.init " 500x500" in
  Graphics.auto_synchronize false;
  Graphics.set_line_width 3;
  MyGraphics.set_x1y1x2y2
    g
    ~x1:(-2.5)
    ~y1:(-2.5)
    ~x2:2.5
    ~y2:2.5;
  let enum = input_lines stdin in
  let enum = Enum.map (fun s -> sscanf s "%g %g %g" (fun t phi theta -> (t, phi, theta))) enum
  in
  let l1 = 1.0 in
  let l2 = 1.0 in
  Enum.iteri
    (fun i (_t, phi, theta) ->
       if i mod 10 = 0 then begin
         let open Flop in
         let x1 = l1 * sin phi in
         let y1 = -l1 * cos phi in
         let x2 = x1 + l2 * sin theta in
         let y2 = y1 - l2 * cos theta in
         (* print_endline (String.concat "\t" (List.map string_of_float [t; x1; y1; x2; y2])); *)
         Graphics.clear_graph ();
         MyGraphics.moveto g 0.0 0.0;
         Graphics.set_color (Graphics.rgb 0 0 255);
         MyGraphics.lineto g x1 y1;
         Graphics.set_color (Graphics.rgb 255 0 0);
         MyGraphics.lineto g x2 y2;
         Graphics.synchronize ();
         Mylib.delay 0.002
       end
    )
    enum
