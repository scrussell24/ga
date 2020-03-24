(* Genetic Algorithm module *)

type 'a ga_config = {
    rand_gene: (unit -> 'a);
    rand_genome: (unit -> 'a list);
    mutation_rate: float;
    pop_size: int;
    fitness: ('a list -> int);
    generations: int
}

let rand_pop config =
  Array.of_list
    (Utils.create_rand_list
       (fun () -> Utils.create_rand_list config.rand_gene 10)
       config.pop_size)  


let fit_sort fit g1 g2 =
  if fit g1 > fit g2 then 1 else -1


let tourney_select pop =
  let (i, j) = Utils.rand_index pop, Utils.rand_index pop in
  if i < j                     
  then Array.get pop i
  else Array.get pop j


let tourney_insert pop chrm =
  let (i, j) = Utils.rand_index pop, Utils.rand_index pop in
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
  (Utils.front_of_list mom i) @ (Utils.back_of_list dad i)


let reproduce config pop =
  let (mom, dad) = tourney_select pop, tourney_select pop in
  let kid = mutate config (crossover mom dad) in
  tourney_insert pop kid;
  Array.sort (fit_sort config.fitness) pop;
  pop


let evolve config =
  let pop = (rand_pop config) in
  Utils.apply
    pop
    (fun pop -> Utils.apply pop (reproduce config) config.pop_size)
    config.generations


let print_genome print_gene fitness c =
  print_string "<";
  print_int (fitness c);
  print_string "|~";
  List.iter (fun g -> print_gene g; print_string "~") c;
  print_string ">"

