
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

let mat_scale s m = Array.map (Array.map (( *. ) s)) m

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
  let r = Array.length m and c = Array.length m.(0) in
  Array.init c (fun j -> Array.init r (fun i -> m.(i).(j)))

let vec_add a b = Array.init (Array.length a) (fun i -> a.(i) +. b.(i))


let vec_scale v s = Array.map ((*.)s)v

let vec_dot a b =
let s = ref 0. in
Array.iteri (fun i x -> s := !s +. x *. b.(i)) a;

let outer u v =
Array.init (Array.length u) (fun i ->
  Array.init (Array.length v) (fun j -> u.(i) *. v.(j)))


let argmax v =
Array.flod_left (fun m i -> if v.(i) > v.(m) then i else m) 0
  (Array.init (Array.length v) Fun.id)


let mat_exp a =
let n = Array.length a in
let norm = Array.fold_left (fun mx row ->
  max mx (Array.fold_left (fun s x -> s +. abs_float x) 0. row)) 0. a in
let s = max 0 (int_of_float (ceil(log (max 1. norm) /. log 2.))) in
let sc = 2. ** float_of_int s in
let a = mat_scale (1. /. sc) a in
let r = ref (mat_id n) in
let k = ref (mat_id n) in
for k = 1 to 20 do
t := mat_scale (1. /. float_of_int k) (mat_mul !t a);
r := mat_add !r !t
done;
for _= 1 to s do r := mat_mul !r !r done;
!r

let () = Random.self_init ()
let randn () =
let u = max Float.epsilon (Random.float 1.) in
sqrt (-2 *. log u) *. cos (2. *. Float.pi *. (Random.float 1.))

let randn_v n s = Array.init n (fun _ -> rand () *. s)
let randn_m r c s = Array.init r (fun _ -> randn_v c s

let hippo n =
Array.init n (fun i -> Array.init n(fun j ->
if i > j then -. sqrt (float_of_int ((2*i+1) * (2*j+1)))
else if i = j then -. float_of_int (i+1)
else 0.))

type ssm = (**)
type linear = (**)

let make_ssm  = (**)

let discretize = (**)

let ssm_fwd = (**)

let ssm_bwd = (**)

let softmax = (**)


type adam = (**)

let new_adam = (**)
