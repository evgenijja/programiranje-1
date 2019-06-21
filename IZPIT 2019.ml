(*1. naloga*)

(*a*)
let podvoji_vsoto x y =
  2 * (x + y)

(*b*)
let povsod_vecji trojica1 trojica2 =
  match trojica1, trojica2 with
  | (x, y, z), (x', y', z') -> 
  x >= x' && y >= y' && z >= z' 
  | _, _ -> failwith "ne morem sploh preveriti"

(*c*)
let uporabi_ce_lahko f x =
  match x with
  | Some x -> Some (f x)
  | _ -> None

(*d*)
(*ta funkcija deluje samo za določen element seznama, 
ki ga podamo kot argument bom popravila če bom imela čas*)
let rec pojavi_dvakrat k xs =
  let rec aux k xs acc =
    match xs with
    | [] -> List.length acc = 2
    | x :: xs' -> 
      if x = k then aux k xs' (acc @ [x])
      else aux k xs acc
    in 
    aux k xs []

let rec pojavi xs =
  match xs with
  | [] -> false
  | x :: xs' -> 
  if pojavi_dvakrat x xs' then true 
  else pojavi xs'

(*e*)
(*zračuna vrednosti za x tipa int*)
let rec izracunaj_v_tocki x f_list =
  let rec aux x f_list acc = 
    match f_list with
    | [] -> acc
    | f :: f_list' -> aux x f_list' (acc @ [f x])
  in
  aux x f_list []

(*f*)
let rec eksponent x p =
  let rec eksponent' x p k =
    if p = 0 then k else
    eksponent' x (p - 1) (k * x)
  in eksponent' x p 1



(*2. naloga*)

(*a*)
type 'a mm_drevo = 
  | Empty
  | Node of 'a * int * 'a mm_drevo * 'a mm_drevo

  let test_tree = Node(2, 2, 
  Node(1, 3, Empty, Empty), 
  Node(5, 1, 
  Node(4, 1, Empty, Empty), 
  Node(8, 2, Empty, Empty)))

(*b*)
let rec vstavi x = function
  | Empty -> Node(x, 1, Empty, Empty)
  | Node(y, stevec, levo, desno) -> 
    if y > x then 
    Node(y, stevec, vstavi x levo, desno)
    else if x > y then 
    Node(y, stevec, levo, vstavi x desno)
    else (*x = y*) 
    Node(y, stevec + 1, levo, desno)

(*c*)
(*ta funkcija dela samo za prvi element*)
let rec multimnozica_iz_seznama xs =
  let rec create_tree' xs new_tree =
  match xs with
  | [] -> new_tree
  | x :: xs' -> 
    vstavi x new_tree 
  in
  let new_tree = Node(x, 1, Empty, Empty) in
  create_tree' xs new_tree
 
let rec multimnozica xs =
  match xs with
  | [] -> Empty
  | x :: xs -> multimnozica_iz_seznama xs  

(*veliko stvari je narobe s tema funkcijama*)
  
(*d*)
let rec velikost_multimnozice neko_drevo = 
  match neko_drevo with
    | Empty -> 0
    | Node(x, y, levo, desno) ->
      y + velikost_multimnozice levo + velikost_multimnozice desno
   
(*e*)
let rec seznam_iz_multimnozice drevo =
  match drevo with
  | Empty -> []
  | Node(x, 0, Empty, Empty) -> []
  | Node(x, 0, levo, desno) -> seznam_iz_multimnozice levo @ seznam_iz_multimnozice desno
  | Node(x, y, levo, desno) ->
  let novo_drevo = Node(x, y-1, levo, desno) in 
    [x] @ seznam_iz_multimnozice novo_drevo;;

