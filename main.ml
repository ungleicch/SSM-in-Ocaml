
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

let mat_scale s m = Array.map (Array.map ((*.)s))m

(* matrix multiplicaiton / using ikj loop more cache ef*)
let mat_mul a b =
  let r = Array.length a
  and k = Array.length b
  and c = Array.length b in
  let out = mat_zeros r c in
  for i = 0 to r - 1 do
    for p = 0 to k - 1 do
      let aip = a.(i).(p) in
      if aip <> 0. then
        for j = 0 to c - 1 do
          out.(i).(j) <- out.(i).(j) +. aip *. b.(p).(j)
        done
    done
  done;
  out

let mat_vec a b =
  let r = Array.length a and c = Array.length v in
  Array.init r (fun i ->
  let s = ref 0. in
  for j = 0 to c - 1 do
  s := !s +. a.(i).(j) *. v.(j)
  done;
  !s)

let mat_t m =
  let r = Array.length m and c


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
