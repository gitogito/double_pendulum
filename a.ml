open Printf
open ExtLib

let pi = 4.0 *. atan 1.0

let m1 = 1.0
let m2 = 1.0
let l1 = 1.0
let l2 = 1.0
let g = 9.8

let x_ary = [|
  0.1 *. pi;  (* phi *)
  0.1 *. pi;  (* theta *)
  0.0;  (* mu = d_phi *)
  0.0;  (* nu = d_theta *)
|]

let n_phi   = 0
let n_theta = 1
let n_mu    = 2
let n_nu    = 3

let xdot_ary =
  let open Flop in
  let _a = (m1+m2)*l1**2.0 in
  let _e = m2*l2 in
  [|
    (* phi *)
    (fun ~t ~x_ary ->
       ignore t;
       x_ary.(n_mu));

    (* theta *)
    (fun ~t ~x_ary ->
       ignore t;
       x_ary.(n_nu));

    (* mu *)
    (fun ~t ~x_ary ->
       ignore t;
       let phi = x_ary.(n_phi) in
       let theta = x_ary.(n_theta) in
       let mu = x_ary.(n_mu) in
       let nu = x_ary.(n_nu) in

       let _b = m2*l1*l2*cos(phi-theta) in
       let _c = m2*l1*l2*sin(phi-theta) in
       let _d = (m1+m2)*g*l1*sin(phi) in
       let _f = m2*l1*l2*cos(phi-theta) in
       let _g = -m2*l1*l2*sin(phi-theta) in
       let _h = m2*g*l2*sin(theta) in
       -1.0/(_a*_e-_f*_b) * (_c*_e*nu**2.0 - _g*_b*mu**2.0 + _d*_e - _h*_b)
    );

    (* nu *)
    (fun ~t ~x_ary ->
       ignore t;
       let phi = x_ary.(n_phi) in
       let theta = x_ary.(n_theta) in
       let mu = x_ary.(n_mu) in
       let nu = x_ary.(n_nu) in

       let _b = m2*l1*l2*cos(phi-theta) in
       let _c = m2*l1*l2*sin(phi-theta) in
       let _d = (m1+m2)*g*l1*sin(phi) in
       let _f = m2*l1*l2*cos(phi-theta) in
       let _g = -m2*l1*l2*sin(phi-theta) in
       let _h = m2*g*l2*sin(theta) in
       -1.0/(_b*_f-_e*_a) * (_c*_f*nu**2.0 - _g*_a*mu**2.0 + _d*_f - _h*_a)
    );
|]

let loop n dt ode =
  let rec aux i =
    if i > n then
      ()
    else begin
      let t = dt *. float i in
      let phi = (ode.Orddiff.Base.get_x_ary ()).(n_phi) in
      let theta = (ode.Orddiff.Base.get_x_ary ()).(n_theta) in
  (*
      let mu = (ode.Orddiff.Base.get_x_ary ()).(n_mu) in
      let nu = (ode.Orddiff.Base.get_x_ary ()).(n_nu) in
   *)
      printf "%g\t%g\t%g\n" t phi theta;
      ode.Orddiff.Base.update ();
      aux (i + 1)
    end
  in
  aux 0

let () =
  let n = 1000 in
  let dt = 0.1 in
  let ode = Orddiff.Rk4.init ~dt ~x_ary ~xdot_ary in
  loop n dt ode
