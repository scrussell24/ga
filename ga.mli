type 'a ga_config = {
  rand_gene : unit -> 'a;
  rand_genome : unit -> 'a list;
  mutation_rate : float;
  pop_size : int;
  fitness : 'a list -> int;
  generations : int;
}
val evolve : 'a ga_config -> 'a list array
val print_genome : ('a -> 'b) -> ('a list -> int) -> 'a list -> unit
