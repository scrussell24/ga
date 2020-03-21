(* Genetic Algorithm implementation in OCAML *)

let pop_size = 100
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


let rec create_rand_list rand_element length =
  match length with
    0 -> []
  | _ -> rand_element () :: create_rand_list rand_element (length - 1)


(* Is there a sum function? *)
let sum_int_list ls = List.fold_left (+) 0 ls


let rand_index a =
  Random.int (Array.length a)


let rand_gene () = Random.int 10


let rand_chrm () = create_rand_list rand_gene chrm_length


let rand_pop size = create_rand_array rand_chrm size  


(* let mutate chrm =
 *   let n = Random.float 1.0 in
 *   if n < mutation_rate
 *   then
 *     let new_chrm = Array.copy chrm in
 *     Array.set new_chrm (rand_index new_chrm) (rand_gene ());
 *     new_chrm
 *   else chrm *)

let rec mutate chrm =
  match chrm with
    [] -> []
  | h::t ->
     if mutation_rate < Random.float 1.0
     then h :: mutate t
     else rand_gene () :: mutate t



let fit_sort fit chrm1 chrm2 =
  if fit chrm1 > fit chrm2 then -1 else 1


let tourney_select pop =
  let (i, j) = rand_index pop, rand_index pop in
  if i < j                     
  then Array.get pop i
  else Array.get pop j


let tourney_insert pop chrm =
  let (i, j) = rand_index pop, rand_index pop in
  if i < j                     
  then Array.set pop j chrm
  else Array.set pop i chrm


let reproduce pop =
  let (mom, dad) = tourney_select pop, tourney_select pop in
  let kid = mutate mom in
  tourney_insert pop kid;
  Array.sort (fit_sort sum_int_list) pop;
  pop


let next_gen pop = apply pop reproduce pop_size


let evolve pop = apply pop next_gen generations


let print_chrm print_gene fitness c =
  print_string "<";
  print_int (fitness c);
  print_string "|~";
  List.iter (fun g -> print_gene g; print_string "~") c;
  print_string ">"


let main =
  let pop = evolve (rand_pop pop_size) in
  print_chrm print_int sum_int_list (Array.get pop 0)
