let rec ena_pomozna_funkcija = function
| [] -> ""
| x :: xs -> string_of_int x ^ ena_pomozna_funkcija xs

let rec izpisi_vsa_stevila' xs = 
 let str = ena_pomozna_funkcija xs in print_endline str

let map2_opt f xs ys =
  if List.length xs = List.length ys then 
  match (xs, ys) with
  | ([], []) -> Some []
  | (x :: xs, y :: ys) -> Some [f x y] @ map2_opt f xs ys
  else
  None

