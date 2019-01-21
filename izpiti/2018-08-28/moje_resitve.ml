(*prva naloga*)


let razlika_kvadratov x y = (x + y) ** 2 - (x ** 2 + y ** 2) 

let rec uporabi_na_paru f (x, y) = (f x, f y)

let rec ponovi_seznam n xs = 
  match xs with
  | [] -> []
  | xs -> if n <= 0 then [] else xs @ ponovi_seznam (n - 1)  xs

let rec razdeli xs = 
  let rec razdeli' n_acc p_acc = function
  | [] -> (List.rev n_acc, List.rev p_acc)
  | x :: xs when x < 0 -> razdeli' (x :: n_acc) p_acc xs
  | x :: xs razdeli' n_acc (x :: p_acc) xs
  in
  razdeli' [] [] xs


(*jej druga naloga*)

(*jej deli in vladaj*)

(*
glej tist primer drevesa: začneš pr 11; lahku je v verigi al pa ni
1. recimo da je
a) padajoča v desnem + 11 + naraščajoča v levem
b) naraščajoča v desnem + 11 + padajoča v levem

2. recimo da ni
greš rekurzijo za levo in za desno
*)

type 'a drevo = Empty | Node of 'a drevo * 'a * 'a drevo

let leaf x = Node (Empty, x, Empty)

let test1 = 
  Node(
    Node(leaf 3, 10, Node(leaf 14, 13, leaf 6)),
    11,
    Node(leaf 2, 8, leaf 10))

let rec padajoca v = function
| Empty -> []
| Node (l, x, r) when v > x -> [] (*iz tle ne boš dobila padajoče verige*)
| Node (l, x, r) ->
  let left = padajoca x l in
  let right = padajoca x r in
  if List.length left > List.length right then 
    x :: left
  else
    x :: right

let rec narascajoca v = function
| Empty -> []
| Node (l, x, r) when v < x -> [] 
| Node (l, x, r) ->
  let left = padajoca x l in
  let right = padajoca x r in
    if List.length left > List.length right then 
      x :: left
    else
      x :: right

let rec monotona_pot = function
| Empty -> []
| Node (l, x, r) ->
  let pure_left = monotona_pot l in
  let pure_right = monotona_pot r in
  let left_to_right = (padajoca x l) @ [x] @ (narascajoca x r) in
  let right_to_left = (padajoca x r) @ [x] @ (narascajoca x l) in
  let options = [pure_right; left_to_right; right_to_left] in
  let pick_bigger x y = if List.length x > List.length y then x else y in
  List.fold_left pick_bigger pure_left options

(*in še tretja jeej*)

type 'a veriga = 
  | Filter of ('a -> bool) * 'a list 'a veriga
  | Ostalo of 'a list

let testni_filter = 
  Filter((fun x -> x > 0), [], 
  Filter(( fun x -> )) 
   
)


let vstavi x veriga =
  match veriga with
  | Ostalo (elementi) -> Ostalo (x :: elementi)
  | Filter (f, element, filtri) -> 
    if x then
      Filter (f, x :: elementi, filtri)
    else
      Filter (f, elementi, vstavi x filtri)

let rec poisci x = function
  | Ostalo elementi -> List.mem x elementi
  | Filter (f, elementi, filtri) -> 
    if f x then List.mem x elementi else poisci x filtri

let rec izprazni = function
| Ostalo elementi -> (Ostalo [], elementi)
| Filter (f, elementi, filtri) -> 
  let prazni_filtri, pobrani_elementi = izprazni_filtri in
  let vsi_elementi = elementi @ pobrani_elementi in
  (Filter (f, [], prazni_filtri), vsi_elementi)

let rec dodaj f veriga = 
  let veriga' = Filter(f, [], veriga) in
  let prazna_veriga, elementi = izprazni veriga' in
  List.fold_left (fun v x -> vstavi x v) prazna_veriga elementi