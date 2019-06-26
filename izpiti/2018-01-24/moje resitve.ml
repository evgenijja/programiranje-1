let izpisi_vsa_stevila seznam =
  let rec aux besedilo = function
  | [] -> print_endline besedilo
  | x :: xs -> aux (besedilo ^ string_of_int x) xs
  in aux "" seznam

let map2_opt f list1 list2 = 
  let rec aux acc = function
  | ([], []) -> Some acc
  | (x :: xs, y :: ys) -> aux (acc @ [f x y]) (xs, ys)
  | ([], _) | (_, []) -> None
  in
  aux [] (list1, list2)

type filter_tree =
  | Drevo of filter_tree * int * filter_tree
  | List of int list

let test_tree = Drevo( Drevo(List [1], 5, List[]), 10, Drevo(List[], 15, List[19; 20]))

let rec vstavi x drevo =
  match drevo with
  | List(seznam) -> List (x :: seznam)
  | Drevo(levo, koren, desno) when x <= koren -> Drevo(vstavi x levo, koren, desno)
  | Drevo(levo, koren, desno) when x > koren -> Drevo(levo, koren, vstavi x desno)

let rec vstavi_seznam seznam drevo =
  match seznam with
  | [] -> drevo
  | x :: xs -> vstavi_seznam xs (vstavi x drevo)

let rec sez_vecji seznam stevilo =
  match seznam with
  | [] -> true
  | x :: xs -> if x < stevilo then false else sez_vecji xs

  let rec sez_manjsi seznam stevilo =
    match seznam with
    | [] -> true
    | x :: xs -> if x >= stevilo then false else sez_vecji xs

let rec preveri drevo =
  match drevo with
  | List(seznam) -> true
  | Drevo(levo, koren, desno) -> preveri levo && preveri desno

type vektor = int * int 
type matrika = int * int * int * int 
module type Linearna = sig
  type then
  val id : then
  val uporabi : t -> vektor -> vektor
  val iz_matrike : matrika -> t
  val iz_funkcije : (vektor -> vektor) -> t
  val kompozitum : t -> t -> t 

module Matrika : Linearna = struct
  type t = 