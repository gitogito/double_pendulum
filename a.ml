open Printf
open ExtLib

let pi = 4.0 *. atan 1.0

let m1 = 1.7
let m2 = 1.0
let l1 = 1.0
let l2 = 1.0
let g = 9.8

let xs = [|
  0.99 *. pi;  (* phi *)
  0.5 *. pi;  (* theta *)
  0.0;  (* mu = d_phi *)
  0.0;  (* nu = d_theta *)
|]

let n_phi   = 0
let n_theta = 1
let n_mu    = 2
let n_nu    = 3

let dxdt _t xs fs =
  let open Flop in

  let phi = xs.(n_phi) in
  let theta = xs.(n_theta) in
  let mu = xs.(n_mu) in
  let nu = xs.(n_nu) in

  let c_a = (m1+m2)*l1**2.0 in
  let c_b = m2*l1*l2*cos(phi-theta) in
  let c_c = m2*l1*l2*sin(phi-theta) in
  let c_d = (m1+m2)*g*l1*sin(phi) in
  let c_e = m2*l2 in
  let c_f = m2*l1*l2*cos(phi-theta) in
  let c_g = -m2*l1*l2*sin(phi-theta) in
  let c_h = m2*g*l2*sin(theta) in

  fs.(n_phi) <- xs.(n_mu);
  fs.(n_theta) <- xs.(n_nu);
  fs.(n_mu) <- -1.0/(c_a*c_e-c_f*c_b) * (c_c*c_e*nu**2.0 - c_g*c_b*mu**2.0 + c_d*c_e - c_h*c_b);
  fs.(n_nu) <- -1.0/(c_b*c_f-c_e*c_a) * (c_c*c_f*nu**2.0 - c_g*c_a*mu**2.0 + c_d*c_f - c_h*c_a)

let loop n ode xs =
  let rec aux i =
    if i > n then
      ()
    else begin
      let t = ode.Orddiff.Base.get_t () in
      printf "%g\t%g\t%g\n" t xs.(n_phi) xs.(n_theta);
      ode.Orddiff.Base.update ();
      aux (i + 1)
    end
  in
  aux 0

let () =
  let n = 1000000 in
  let dt = 0.001 in
  let ode = Orddiff.Rk4.init ~dt ~dxdt ~xs in
  loop n ode xs
