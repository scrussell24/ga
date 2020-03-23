(* Non ga specific helper functions *)

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
