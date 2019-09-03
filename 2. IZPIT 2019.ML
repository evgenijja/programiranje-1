(*1. NALOGA*)

type complex = {re : float; im : float}

(*a*)
let complex_add x y = x.re +. y.re +. x.im +. y.im

(*b*)
let complex_conjugate {re; im} = {re; im = -. im}

(*c*)
let rec list_apply_either pred f g list =
  match list with
  | [] -> []
  | x :: xs -> if pred x then (f x) :: list_apply_either pred f g xs else (g x) :: list_apply_either pred f g xs

(*d*)
let rec potenciraj x potenca = if potenca != 0 then x * potenciraj x (potenca - 1) else 1 

let eval_poly polinom tocka =
  let rec aux sestevek potenca = function
  | [] -> sestevek
  | x :: xs -> aux (sestevek + x * (potenciraj tocka potenca)) (potenca + 1) xs
  in
  aux 0 0 polinom

(*2. NALOGA*)

type najemnik = string

type vrt =
  | Obdelovan of najemnik
  | Oddan of najemnik * (vrt * vrt list)
  | Prost

(*a*)
let vrt_primer = Oddan("Kovalevskaya", (Obdelovan("Galois"), [Obdelovan("Lagrange"); Prost]))
let vrt_primer2 = Oddan("Kovalevskaya", (Prost, [Obdelovan("Lagrange"); (Obdelovan("Galois"))]))

(*b*)
let obdelovalec_vrta = function
| Obdelovan(najemnik) -> Some najemnik
| Oddan(najemnik, (vrt, vrt_list)) -> None
| Prost -> None

(*c*) 
let rec globina_oddajanja = function
| Obdelovan(najemnik) -> 0
| Prost -> 0
| Oddan(najemnik, (vrt, vrt_list)) ->
  let rec najglobji_od_vrtov list_vrtov =
    match list_vrtov with
    | [] -> 0
    | x :: xs -> max (globina_oddajanja x) (najglobji_od_vrtov xs)
    in
    1 + max (globina_oddajanja vrt) (najglobji_od_vrtov vrt_list)

(*d*)
let rec v_uporabi = function
| Prost -> false
| Obdelovan(najemnik) -> true
| Oddan(najemnik, (vrt, vrt_list)) -> if v_uporabi vrt then true else 
    let rec preveri_list list_vrtov =
      match list_vrtov with
      | [] -> false
      | x :: xs -> if v_uporabi x then true else preveri_list xs
      in
      preveri_list vrt_list

(*e*)
let rec vsi_najemniki = function
| Prost -> []
| Obdelovan(najemnik) -> [najemnik]
| Oddan(najemnik, (vrt, vrt_list)) -> 
let rec se_vec_najemnikov list_vrtov =
  match list_vrtov with
  | [] -> []
  | x :: xs -> (vsi_najemniki x) @ se_vec_najemnikov xs
  in [najemnik] @ (vsi_najemniki vrt) @ (se_vec_najemnikov vrt_list)

(*f*)
let rec vsi_obdelovalci = function
| Prost -> []
| Obdelovan(najemnik) -> [najemnik]
| Oddan(najemnik, (vrt, vrt_list)) -> 
let rec obdelovalci list_vrtov =
  match list_vrtov with
  | [] -> []
  | x :: xs -> (vsi_obdelovalci x) @ (obdelovalci xs)
  in vsi_obdelovalci vrt @ obdelovalci vrt_list

