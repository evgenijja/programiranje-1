(* ========== Vaja 2: Funkcijsko Programiranje  ========== *)

(*----------------------------------------------------------------------------*]
Namig: Definirajte pomožno funkcijo za obračanje seznamov.
[*----------------------------------------------------------------------------*)

<<<<<<< HEAD
(*more bit repno rekurzivna če ne je ne smeš uporablat 
v repno rekurzivnih funkcijah*)

let rec obrni xs =
  let rec obrni' acc = function
  | [] -> acc
  | x :: xs -> obrni' (x :: acc) xs
  in 
  obrni' [] xs
=======
let reverse list =
  let rec reverse_aux acc = function
    | [] -> acc
    | x :: xs -> reverse_aux (x :: acc) xs
  in reverse_aux [] list
>>>>>>> d51c008feea0280026afd13c8e7055619866cb44

(*----------------------------------------------------------------------------*]
 Funkcija [repeat x n] vrne seznam [n] ponovitev vrednosti [x]. Za neprimerne
 vrednosti [n] funkcija vrne prazen seznam.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # repeat "A" 5;;
 - : string list = ["A"; "A"; "A"; "A"; "A"]
 # repeat "A" (-2);;
 - : string list = []
[*----------------------------------------------------------------------------*)

<<<<<<< HEAD
let rec repeat x n =
  if n <= 0 then
   []
  else
  x :: repeat x (n - 1)



(*tale ponovi usako št s seznama*)
let rec repeat xs = 
  let rec repeat' acc = function
  | [] -> acc
  | x :: xs -> repeat' (acc @ [x] @ [x]) xs
  in 
  repeat' [] xs

(*pazi katera funkcija je tail recursive!!*)

let rec repeat2  x n =
  let rec repeat2' x n acc =
    if n <= 0 then
      acc
    else
      repeat2' x (n - 1) (x :: acc)
  in
  repeat2' x n []

=======
let rec repeat x n = if n <= 0 then [] else x :: (repeat x (n-1))
>>>>>>> d51c008feea0280026afd13c8e7055619866cb44

(*----------------------------------------------------------------------------*]
 Funkcija [range] sprejme število in vrne seznam vseh celih števil od 0 do
 vključno danega števila. Za neprimerne argumente funkcija vrne prazen seznam.
 Funkcija je repno rekurzivna.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # range 10;;
 - : int list = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
[*----------------------------------------------------------------------------*)

<<<<<<< HEAD
let rec range n =
  if n < 0 then 
    []
  else
    (range (n - 1)) @ [n]

(*še repno rekurzivno:*)

let rec range2 n =
  let rec range2' n acc =
    if n < 0 then
      acc
    else
      range2' (n - 1) (n :: acc) 
  in
  range2' n []
=======
let range_not_tailrec n =
  let rec range_from m =
    if m > n
    then []
    else m :: (range_from (m + 1))
  in range_from 0

let range n =
   let rec range_aux n acc =
     if n < 0 then acc else range_aux (n - 1) (n :: acc)
   in
   range_aux n []
>>>>>>> d51c008feea0280026afd13c8e7055619866cb44

(*----------------------------------------------------------------------------*]
 Funkcija [map f list] sprejme seznam [list] oblike [x0; x1; x2; ...] in
 funkcijo [f] ter vrne seznam preslikanih vrednosti, torej
 [f(x0); f(x1); f(x2); ...].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let plus_two = (+)2 in
   map plus_two [0; 1; 2; 3; 4];;
 - : int list = [2; 3; 4; 5; 6]
[*----------------------------------------------------------------------------*)

let rec map f = function
<<<<<<< HEAD
| [] -> []
| x :: xs -> f x :: map f xs

let rec tlrc_map f xs =
  let rec map' f xs acc =
    match xs with
    | [] -> acc
    | x :: xs -> map' f xs (acc @ [f x])
  in
  map' f xs []

=======
  | [] -> []
  | x :: xs -> f x :: (map f xs)
>>>>>>> d51c008feea0280026afd13c8e7055619866cb44

(*----------------------------------------------------------------------------*]
 Funkcija [map_tlrec] je repno rekurzivna različica funkcije [map].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let plus_two = (fun x -> x+2) in
   map_tlrec plus_two [0; 1; 2; 3; 4];;
 - : int list = [2; 3; 4; 5; 6]
[*----------------------------------------------------------------------------*)

<<<<<<< HEAD
(*tu ne dela ): *)

let plus_two = (fun x -> x+2)

let rec map_tlrec f xs = 
  let rec map_tlrec' f acc = function
  | [] -> acc
  | x :: xs -> 
    let new_acc = f x :: acc in
    map_tlrec' f xs new_acc
  in
  map_tlrec' f []


=======
let map_tlrec f list =
  let rec map_aux list acc =
    match list with
    | [] -> reverse acc
    | x :: xs -> map_aux xs (f x :: acc)
  in
  map_aux list []
>>>>>>> d51c008feea0280026afd13c8e7055619866cb44

(*----------------------------------------------------------------------------*]
 Funkcija [mapi] sprejme seznam in funkcijo dveh argumentov ter vrne seznam
 preslikanih vrednosti seznama, kjer kot drugi argument funkcije podamo indeks
 elementa v seznamu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # mapi (+) [0; 0; 0; 2; 2; 2];;
 - : int list = [0; 1; 2; 5; 6; 7]
[*----------------------------------------------------------------------------*)

<<<<<<< HEAD

let rec mapi f xs =
  let rec mapi' f xs acc = 
  match xs with
  | [] -> acc
  | x :: xs ->
    let new_acc = (f x (List.length acc)) :: acc in
    mapi' f xs new_acc
  in
  mapi' f xs []

  let mapi f list =
    let rec mapi_aux list i =
      match list with
      | [] -> []
      | x :: xs -> (f i x) :: (mapi_aux xs (i + 1))
    in
  mapi_aux list 0

=======
let mapi f list =
  let rec mapi_aux list i =
    match list with
    | [] -> []
    | x :: xs -> (f i x) :: (mapi_aux xs (i + 1))
  in
  mapi_aux list 0
>>>>>>> d51c008feea0280026afd13c8e7055619866cb44

(*----------------------------------------------------------------------------*]
 Funkcija [zip] sprejme dva seznama in vrne seznam parov istoležnih
 elementov podanih seznamov. Če seznama nista enake dolžine vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # zip [1; 1; 1; 1] [0; 1; 2; 3];;
 - : (int * int) list = [(1, 0); (1, 1); (1, 2); (1, 3)]
 # zip [1; 1; 1; 1] [1; 2; 3; 4; 5];;
 Exception: Failure "Different lengths of input lists.".
[*----------------------------------------------------------------------------*)

<<<<<<< HEAD
let rec zip xs1 xs2 =
  if List.length xs1 <> List.length xs2 
  failwith ":("
  else
  let rec zip' xs1 xs2 acc =
    match xs1 with
    | [] -> [] 
    | x :: xs ->
=======
let rec zip list1 list2 =
  match list1, list2 with
  | [], [] -> []
  | _, [] | [], _ -> failwith "Different lengths of input lists."
  | x :: xs, y :: ys -> (x, y) :: (zip xs ys)
>>>>>>> d51c008feea0280026afd13c8e7055619866cb44

(*----------------------------------------------------------------------------*]
 Funkcija [zip_enum_tlrec] sprejme seznama [x_0; x_1; ...] in [y_0; y_1; ...]
 ter vrne seznam [(0, x_0, y_0); (1, x_1, y_1); ...]. Funkcija je repno
 rekurzivna. Če seznama nista enake dolžine vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # zip_enum_tlrec ["a"; "b"; "c"] [7; 3; 4];;
 - : (int * string * int) list = [(0, "a", 7); (1, "b", 3); (2, "c", 4)]
[*----------------------------------------------------------------------------*)

let zip_enum_tlrec list1 list2 =
  let rec zipe_aux list1 list2 i acc =
    match list1, list2 with
    | [], [] -> reverse acc
    | _, [] | [], _ -> failwith "Different lengths of input lists."
    | x :: xs, y :: ys ->
      let element = (i, x, y) in
      zipe_aux xs ys (i + 1) (element :: acc)
   in
   zipe_aux list1 list2 0 []

(*----------------------------------------------------------------------------*]
 Funkcija [unzip] je inverz funkcije [zip], torej sprejme seznam parov
 [(x0, y0); (x1, y1); ...] in vrne par seznamov ([x0; x1; ...], [y0; y1; ...]).
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # unzip [(0,"a"); (1,"b"); (2,"c")];;
 - : int list * string list = ([0; 1; 2], ["a"; "b"; "c"])
[*----------------------------------------------------------------------------*)

let rec unzip = function
  | [] -> ([], [])
  | (x, y) :: tl -> let (list1, list2) = unzip tl in (x :: list1, y :: list2)

(*----------------------------------------------------------------------------*]
 Funkcija [unzip_tlrec] je repno rekurzivna različica funkcije [unzip].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # unzip_tlrec [(0,"a"); (1,"b"); (2,"c")];;
 - : int list * string list = ([0; 1; 2], ["a"; "b"; "c"])
[*----------------------------------------------------------------------------*)

let unzip_tlrec list =
  let rec unzip_aux list acc1 acc2 =
    match list with
    | [] -> (reverse acc1, reverse acc2)
    | (x, y) :: tl -> unzip_aux tl (x :: acc1) (y :: acc2)
  in
  unzip_aux list [] []

(*----------------------------------------------------------------------------*]
 Funkcija [fold_left_no_acc f list] sprejme seznam [x0; x1; ...; xn] in
 funkcijo dveh argumentov [f] in vrne vrednost izračuna
 f(... (f (f x0 x1) x2) ... xn).
 V primeru seznama z manj kot dvema elementoma vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # fold_left_no_acc (^) ["F"; "I"; "C"; "U"; "S"];;
 - : string = "FICUS"
[*----------------------------------------------------------------------------*)

<<<<<<< HEAD

let rec fold_left_no_acc f = function
  | [] | _ :: [] -> failwith "List too short."
  | x :: y :: [] -> f x y
| x :: y :: tl -> fold_left_no_acc f ((f x y) :: tl)
=======
let rec fold_left_no_acc f = function
  | [] | _ :: [] -> failwith "List too short."
  | x :: y :: [] -> f x y
  | x :: y :: tl -> fold_left_no_acc f ((f x y) :: tl)
>>>>>>> d51c008feea0280026afd13c8e7055619866cb44

(*----------------------------------------------------------------------------*]
 Funkcija [apply_sequence f x n] vrne seznam zaporednih uporab funkcije [f] na
 vrednosti [x] do vključno [n]-te uporabe, torej
 [x; f x; f (f x); ...; (f uporabljena n-krat na x)].
 Funkcija je repno rekurzivna.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # apply_sequence (fun x -> x * x) 2 5;;
 - : int list = [2; 4; 16; 256; 65536; 4294967296]
 # apply_sequence (fun x -> x * x) 2 (-5);;
 - : int list = []
[*----------------------------------------------------------------------------*)

<<<<<<< HEAD
let rec apply_sequence f x n =
  let rec seq' f x n acc = 
    match xs with
    | [] -> acc
    | x :: xs -> f x :: acc 

    let apply_sequence f x n =
      let rec apply_aux f x n acc =
        if n < 0 then
          reverse acc
        else
          apply_aux f (f x) (n - 1) (x :: acc)
      in
    apply_aux f x n []
=======
let apply_sequence f x n =
  let rec apply_aux f x n acc =
    if n < 0 then
      reverse acc
    else
      apply_aux f (f x) (n - 1) (x :: acc)
  in
  apply_aux f x n []
>>>>>>> d51c008feea0280026afd13c8e7055619866cb44

(*----------------------------------------------------------------------------*]
 Funkcija [filter f list] vrne seznam elementov [list], pri katerih funkcija [f]
 vrne vrednost [true].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # filter ((<)3) [0; 1; 2; 3; 4; 5];;
 - : int list = [4; 5]
[*----------------------------------------------------------------------------*)

let rec filter f = function
  | [] -> []
  | x :: xs -> if f x then x :: (filter f xs) else filter f xs

(*----------------------------------------------------------------------------*]
 Funkcija [exists] sprejme seznam in funkcijo, ter vrne vrednost [true] čim
 obstaja element seznama, za katerega funkcija vrne [true] in [false] sicer.
 Funkcija je repno rekurzivna.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # exists ((<)3) [0; 1; 2; 3; 4; 5];;
 - : bool = true
 # exists ((<)8) [0; 1; 2; 3; 4; 5];;
 - : bool = false
[*----------------------------------------------------------------------------*)

<<<<<<< HEAD
let rec exists f xs =
  let rec exists' f xs acc =
    match xs with
    | [] -> false
    | x :: xs -> if f x then true else exists' f xs acc
  in 
  exists' f xs []
=======
let rec exists f = function
  | [] -> false
  | x :: xs -> if f x then true else exists f xs
>>>>>>> d51c008feea0280026afd13c8e7055619866cb44

(*----------------------------------------------------------------------------*]
 Funkcija [first f default list] vrne prvi element seznama, za katerega
 funkcija [f] vrne [true]. Če takšnega elementa ni, vrne [default].
 Funkcija je repno rekurzivna.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # first ((<)3) 0 [1; 1; 2; 3; 5; 8];;
 - : int = 5
 # first ((<)8) 0 [1; 1; 2; 3; 5; 8];;
 - : int = 0
[*----------------------------------------------------------------------------*)

let rec first f default = function
<<<<<<< HEAD
| [] -> default
| x :: xs -> if f x then x else first default xs



=======
  | [] -> default
  | x :: xs -> if f x then x else first f default xs
>>>>>>> d51c008feea0280026afd13c8e7055619866cb44
