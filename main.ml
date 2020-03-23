(* Genetic Algorithm implementation in OCAML *)

(* general helper functions *)

(* apply fn to a n times *)
let rec apply a fn n =
  if n <= 0 then a else apply (fn a) fn (n - 1)

(* create a list with elements created by rand_element function *)
let rec create_rand_list rand_element length =
  match length with
    0 -> []
  | _ -> rand_element () :: create_rand_list rand_element (length - 1)

(* return a list of all elements before and including the ith *)
let rec front_of_list ls i =
  match ls with
    [] -> []
  | h::t ->
     if i <= 0
     then [h]
     else h :: front_of_list ls (i - 1)

(* return a list of all elements after the ith *)
let rec back_of_list ls i =
  match ls with
    [] -> []
  | h::t ->
     if i <= 0
     then t
     else back_of_list t (i - 1)

(* returns and random array index *)
let rand_index a =
  Random.int (Array.length a)


(* GA specific *)
type 'a config = {
    rand_gene: (unit -> 'a);
    rand_genome: (unit -> 'a list);
    mutation_rate: float;
    pop_size: int;
    fitness: ('a list -> int);
    generations: int
}

let rand_pop config =
  Array.of_list
    (create_rand_list
       (fun () -> create_rand_list config.rand_gene 10)
       config.pop_size)  


let fit_sort fit g1 g2 =
  if fit g1 > fit g2 then 1 else -1


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


let rec mutate config genome =
  match genome with
    [] -> []
  | h::t ->
     if config.mutation_rate < Random.float 1.0
     then h :: mutate config t
     else config.rand_gene () :: mutate config t


let crossover mom dad =
  let i = Random.int (List.length mom) in
  (front_of_list mom i) @ (back_of_list dad i)


let reproduce config pop =
  let (mom, dad) = tourney_select pop, tourney_select pop in
  let kid = mutate config (crossover mom dad) in
  tourney_insert pop kid;
  Array.sort (fit_sort config.fitness) pop;
  pop


let evolve config =
  let pop = (rand_pop config) in
  apply
    pop
    (fun pop -> apply pop (reproduce config) config.pop_size)
    config.generations


let print_genome print_gene fitness c =
  print_string "<";
  print_int (fitness c);
  print_string "|~";
  List.iter (fun g -> print_gene g; print_string "~") c;
  print_string ">"


let main =
  let rand_gene () = Random.int 10 in
  let fitness gs = List.fold_left (+) 0 gs in
  let config = {
    rand_gene=rand_gene;
    rand_genome=(fun () -> create_rand_list rand_gene 10);
    mutation_rate=0.1;
    pop_size=100;
    fitness=fitness;
    generations=100
  } in
  let pop = evolve config in
  print_genome print_int fitness (Array.get pop 0)
