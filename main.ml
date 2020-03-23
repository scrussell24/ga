

let main =
  let rand () = Random.int 10 in
  let fitness gs = 1000 - List.fold_left (+) 0 gs in
  let pop = Ga.evolve
              rand
              (fun () -> Utils.create_rand_list rand 10)
              0.1
              100
              fitness
              100 in
  Ga.print_genome print_int fitness (Array.get pop 0)
