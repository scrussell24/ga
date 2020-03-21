(* Genetic Algorithm implementation in OCAML *)

let pop_size = 10
let chrm_length = 10
let generations = 100
let mutation_rate = 0.25


(* general helper functions *)
(* not built in? *)
let rec apply a fn n =
  if n <= 0 then a else apply (fn a) fn (n - 1)


(* This is kinda weird *)
let create_rand_array rand_element length =
  Array.map
    (fun x -> rand_element ())
    (Array.make length (rand_element ()))


(* Is there a sum function? *)
let sum_int_array a = Array.fold_left (+) 0 a


let rand_index a =
  Random.int (Array.length a)


let rand_gene () = Random.int 10


let rand_chrm () = create_rand_array rand_gene chrm_length


let rand_pop size = create_rand_array rand_chrm size  


let mutate chrm =
  let n = Random.float 1.0 in
  if n < mutation_rate
  then
    let new_chrm = Array.copy chrm in
    Array.set new_chrm (rand_index new_chrm) (rand_gene ());
    new_chrm
  else chrm


let fit chrm1 chrm2 =
  if sum_int_array chrm1 > sum_int_array chrm2 then -1 else 1


let tourney_select pop =
  let (i, j) = rand_index pop, rand_index pop in
  if i < j                      (* select from lower index *)
  then Array.get pop i
  else Array.get pop j


let tourney_insert pop chrm =
  let (i, j) = rand_index pop, rand_index pop in
  if i < j                      (* insert into the higher index *)
  then Array.set pop j chrm
  else Array.set pop i chrm


let mingle pop =
  let (mom, dad) = tourney_select pop, tourney_select pop in
  let kid = mutate mom in
  tourney_insert pop kid;
  Array.sort fit pop;
  pop


let evolve pop = apply pop mingle pop_size


let print_chrm c =
  print_int (sum_int_array c); print_string ": <";
  Array.iter (fun g -> print_int g; print_string " ") c;
  print_string ">\n"


let print_pop p =
  print_string "population:\n";
  Array.iter (fun c -> print_chrm c) p


let pop = apply (rand_pop pop_size) evolve generations

