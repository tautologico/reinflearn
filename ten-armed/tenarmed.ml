(*
  Code for the 10-armed bandit problem. 

  Mostly based on the algorithms in the book
  Reinforcement Learning, by Sutton and Barto

  Andrei de A. Formiga, 2011-07-17
*)

(*
  Compile with 

  ocamlfind ocamlc -o tenarmed tenarmed.ml -package gsl -linkpkg
*)

open Gsl_rng

(** The random number generator (from GSL). *)
let rng = make TAUS

(** Number of arms *)
let n_arms = 10

(** Generate n normally-distributed numbers with mean 0 and variance sd^2. *)
let rnorm n sd = 
  Array.init n (fun _ -> Gsl_randist.gaussian rng ~sigma:sd)

(** Generate a value for Q*(a). *)
let sample_qstar () = 
  Gsl_randist.gaussian rng ~sigma:1.0 

(** Generate the rewards for n_arms actions, randomly selected from a 
    Gaussian distribution with mean qs and variance 1. *)
let generate_rewards qs = 
  Array.init n_arms (fun _ -> (Gsl_randist.gaussian rng ~sigma:1.0) +. qs) 

let action_val_init value = 
  Array.init n_arms (fun _ -> value)

(** Default initialization of action values. *)
let default_action_val_init () = 
  action_val_init 0.0

(** Optimistic initialization of action values. *)
let optimistic_action_val_init () = 
  action_val_init 5.0 

(** Get the max value and index of the max value in an array *)
let array_max_idx a = 
  let m = ref a.(0) and ix = ref 0 in
  Array.iteri (fun i x -> if x > !m then (m := x; ix := i) else ()) a;
  (!m, !ix)

(** A single play. *)
let play k r q a = 
  let qmax, ix = array_max_idx q in   (* greedy action selection *)
  let rkp1 = r.(ix) in
  a.(ix) <- a.(ix) + 1;
  q.(ix) <- q.(ix) +. (1.0 /. ((float k) +. 1.0)) *. (rkp1 -. q.(ix)) (* incremental update *)
  
(* Print an array *)

let print_float_array a = 
  let l = Array.length a in
  for i = 0 to l - 2 do
    Printf.printf "%f, " a.(i)
  done; 
  Printf.printf "%f\n" a.(l-1)

let print_int_array a = 
  let l = Array.length a in
  for i = 0 to l - 2 do
    Printf.printf "%d, " a.(i)
  done; 
  Printf.printf "%d\n" a.(l-1)

(** Initialize a task. *)
let init_task () = 
  let qs = sample_qstar () in
  let r = generate_rewards qs in
  let q = optimistic_action_val_init () in
  let acts = Array.init n_arms (fun _ -> 0) in
  print_float_array r;
  for k = 1 to 1000 do
    play k r q acts 
  done;
  print_float_array q;
  print_int_array acts

(* small test *)
let _ = 
  set rng 10101n; 
  print_endline "A single 10-armed task";
  init_task ()
