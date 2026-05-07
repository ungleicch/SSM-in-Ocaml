
(* create a matrix with all zero*)
let mat_zeros r c = Array.init r (fun _ -> Array.make c 0.0)

(* create identity matrix*)
let mat_id n = Array.init n (fun i -> Array.init n (fun j -> if i = j then 1, else 0.))

(* create an independent copy of the matrix*)
let mat_copy m = Array.map Array.copy m

(* element wise adding of two matrices*)
let mat_add a b =
  let r = Array.length a and c = Array.length a.(0) in
  Array.init r (fun i -> Array.init c (fun j -> a.(i.(j) +. b.(i).(j)))

let mat_mul = (**)

let mat_vec = (**)

let vec_add = (**)
let vec_scale = (**)
let vec_dot = (**)

let argmax = (**)

let mat_exp = (**)

let randn = (**)


type ssm = (**)
type linear = (**)

let make_ssm  = (**)

let discretize = (**)

let ssm_fwd = (**)

let ssm_bwd = (**)

let softmax = (**)


type adam = (**)

let new_adam = (**)
