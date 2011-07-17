(*
  Code for the 10-armed bandit problem. 

  Mostly based on the algorithms in the book
  Reinforcement Learning, by Sutton and Barto

  Andrei de A. Formiga, 2011-07-17
*)

open Gsl_rng

let rng = make TAUS

let rnorm n sd = 
  let a = Array.make n 0.0 in
  let init () = Array.iteri (fun i _ -> a.(i) <- Gsl_randist.gaussian rng ~sigma:sd) a in
  init (); 
  a

(* Generate a value for Q*(a) *)
let sample_qstar () = 
  Gsl_randist.gaussian rng ~sigma:1.0 

(* small test *)
let _ = 
  let rgauss = rnorm 20 1.0 in
  Array.iter (Printf.printf "%f\n") rgauss

