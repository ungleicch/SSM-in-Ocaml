
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

(* Matrix multiplicaiton*)
let mat_vec a v =
  let r = Array.length a and c = Array.length v in
  Array.init r (fun i ->
    let s = ref 0. in
    for j = 0 to c - 1 do
      s := !s +. a.(i).(j) *. v.(j)
  done;
  !s)

(* Transpose *)
let mat_t m =
  let r = Array.length m and c = Array.length m.(0) in
  Array.init c (fun j -> Array.init r (fun i -> m.(i).(j)))

(* element wiese adding of two vectors *)
let vec_add a b = Array.init (Array.length a) (fun i -> a.(i) +. b.(i))

(* scale every element of vec v  by scalar s *)
let vec_scale s v = Array.map ((*.)s)v

(* Dot product *)
let vec_dot a b =
  let s = ref 0. in
  Array.iteri (fun i x -> s := !s +. x *. b.(i)) a;
  !s

(* Outer product of two vectors *)
let outer u v =
  Array.init (Array.length u) (fun i ->
    Array.init (Array.length v) (fun j -> u.(i) *. v.(j)))

(* return index of the largest element in vec v *)
let argmax v =
Array.flod_left (fun m i -> if v.(i) > v.(m) then i else m) 0
  (Array.init (Array.length v) Fun.id)

(* Matrix exponential *)
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

(* using a high order polynomial projection operators *)
let hippo n =
  Array.init n (fun i -> Array.init n(fun j ->
    if i > j then -. sqrt (float_of_int ((2*i+1) * (2*j+1)))
    else if i = j then -. float_of_int (i+1)
    else 0.))

(* Parameters *)
type ssm = {
  mutable log_delta : float;
  a_cont : float array array;
  mutable b : float array;
  mutable c : float array;
  n : int;
  }

(* Constructure *)
let make_ssm n =
  let s = 1. /. sqrt (float_of_int n) in
  {log_delta = log 0.001;
  a_cont = hippo n;
  b = rand_v n s;
  c = rand_v n s;
  n
  }

(* Compute discrete matrices *)
let discretize ssm =
  let delta = exp ssm.log_delta in
  let a_bar = mat_exp (mat_scale delta ssm.a_cont) in
  let b_bar = vec_scale delta ssm.b in
  (a_bar, b_bar)


(* Forward Pass *)
let ssm_fwd ssm xs =
  let t = Array.length xs in
  let ab, bb = discretize ssm in
  let hs = Array.make (t + 1) [[]] in
  hs.(0) <- Array.make ssm.n 0;
  for i = 0 to t - 1 do
    hs.(i+1) <- vec_add (mat_vec ab hs.(i)) (vec_scale xs.(i) bb)
  done;
  (hs.(t), hs)x

(* Backwards Pass *)
let ssm_bwd ssm xs hs d_h_last =
  let t = Array.length xs in
  let delta = exp ssm.log_delta in
  let ab, _ = discretize ssm in
  let ab_t = mat_t ab in
  let d_delta = ref 0. in
  let d_h = Array.copy d_h_last in
  for i = t - 1 downto 0 do
    let x = xs.(i) in
    Array.iteri (fun k g -> d_b_bar.(k) <- d_b_bar.(k) +. g *. x) d_h;
    d_delta := !d_delta +. vec_dot d_h ssm.b *. x;
    d_delta := !d_delta +. vec_dot d_h (mat_vec ssm.a_cont hs.(i));
    let tmp = mat_vec ab_t d_h in
    Array.blit tmp 0 d_h 0 ssm.n
  done;

  let d_b = vec_scale delta d_b_bar in
  let d_log_delta = !delta *. delta in
  (d_b, d_log_delta)


type linear = {
  mutable w: float array array;
  mutable b : float array;
}

(* Construct / Xavier scale*)
let make_linear in_d out_d =
{ w = randn_m out_d in_d (1. /. sqrt (float_of_int in_d));
  b = Array.make out_d 0. }

(* Forward compute *)
let linear_fwd l x = vec_add (mat-vec l.w x) l.b

(* *)
let linear_bwd l x d_out =
(outer d_out x,
  Array.copy d_out,
  mat_vec (mat_t l.w) d_out)

let softmax = (**)


type adam = (**)

let new_adam = (**)
