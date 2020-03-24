open Ga


let main =
  let rand_gene () = Random.int 10 in
  let fitness gs = 1000 - List.fold_left (+) 0 gs in
  let pop = Ga.evolve {
                rand_gene=rand_gene;
                rand_genome=(fun () -> Utils.create_rand_list rand_gene 10);
                mutation_rate=0.1;
                pop_size=100;
                fitness=fitness;
                generations=100
              } in
  Ga.print_genome print_int fitness (Array.get pop 0)
