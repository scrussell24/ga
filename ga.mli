val evolve :
  (unit -> 'a) ->
  (unit -> 'a list) ->
  float -> int -> ('a list -> int) -> int -> 'a list array
val print_genome : ('a -> 'b) -> ('a list -> int) -> 'a list -> unit
