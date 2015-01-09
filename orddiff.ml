module Base = struct
  type t = {
    update : unit -> unit;
    get_x_ary : unit -> float array;
  }
end

module Euler = struct
  include Base

  type t' = {
    dt : float;
    mutable n : int;
    mutable x_ary : float array;
    xdot_ary : (t:float -> x_ary:float array -> float) array;
  }

  let update self () =
    let t = self.dt *. float self.n in
    self.x_ary <- Array.mapi
                    (fun i x -> x +. (self.xdot_ary.(i) ~t ~x_ary:self.x_ary) *. self.dt)
                    self.x_ary;
    self.n <- self.n + 1

  let get_x_ary self () =
    self.x_ary

  let init ~dt ~x_ary ~xdot_ary =
    let self = {
      dt;
      n = 0;
      x_ary;
      xdot_ary;
    } in
    { update = update self;
      get_x_ary = get_x_ary self; }
end

module Rk4 = struct
  include Base

  type t' = {
    dt : float;
    mutable n : int;
    mutable x_ary : float array;
    xdot_ary : (t:float -> x_ary:float array -> float) array;
    order : int;
    mutable f1_ary : float array;
    mutable f2_ary : float array;
    mutable f3_ary : float array;
    mutable f4_ary : float array;
    dt2 : float;
  }

  let update self () =
    let t = self.dt *. float self.n in

    for i = 0 to self.order - 1 do
      self.f1_ary.(i) <- self.xdot_ary.(i) ~t ~x_ary:self.x_ary
    done;

    let x_ary = Array.mapi (fun i x -> x +. self.dt2 *. self.f1_ary.(i)) self.x_ary in
    for i = 0 to self.order - 1 do
      self.f2_ary.(i) <- self.xdot_ary.(i) ~t:(t +. self.dt2) ~x_ary
    done;

    let x_ary = Array.mapi (fun i x -> x +. self.dt2 *. self.f2_ary.(i)) self.x_ary in
    for i = 0 to self.order - 1 do
      self.f3_ary.(i) <- self.xdot_ary.(i) ~t:(t +. self.dt2) ~x_ary
    done;

    let x_ary = Array.mapi (fun i x -> x +. self.dt *. self.f3_ary.(i)) self.x_ary in
    for i = 0 to self.order - 1 do
      self.f4_ary.(i) <- self.xdot_ary.(i) ~t:(t +. self.dt) ~x_ary
    done;

    for i = 0 to self.order - 1 do
      let f1 = self.f1_ary.(i) in
      let f2 = self.f2_ary.(i) in
      let f3 = self.f3_ary.(i) in
      let f4 = self.f4_ary.(i) in
      self.x_ary.(i) <- self.x_ary.(i) +. self.dt /. 6.0 *. (f1 +. 2.0 *. f2 +. 2.0 *. f3 +. f4)
    done;

    self.n <- self.n + 1

  let get_x_ary self () =
    self.x_ary

  let init ~dt ~x_ary ~xdot_ary =
    let order = Array.length x_ary in
    let self = {
      dt;
      n = 0;
      x_ary;
      xdot_ary;
      order;
      f1_ary = Array.make order 0.0;
      f2_ary = Array.make order 0.0;
      f3_ary = Array.make order 0.0;
      f4_ary = Array.make order 0.0;
      dt2 = dt /. 2.0
    } in
    { update = update self;
      get_x_ary = get_x_ary self; }
end
