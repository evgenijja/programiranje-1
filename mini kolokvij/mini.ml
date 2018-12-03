(* -------- 1 -------- *)

let rec vsota xs = 
  let rec vsota' acc = function
  | [] -> acc
  | x :: xs -> vsota' (acc + x) xs
  in
vsota' 0 xs


(* -------- 2 -------- *)

let rec get k xs =
  match xs with
  | [] -> 0
  | x :: xs ->
    if k <= 0 then
      x
    else
      get (k-1) xs

let rec urejen_seznam xs = function
| [] -> "Je urejen"
| x :: xs -> 
  if x <= (get 0 xs) then urejen_seznam xs
  else "Ni urejen"


  

(* -------- 3 -------- *)

let vstavi y = function
| [] -> y :: []
| x :: xs -> y :: x :: xs

let rec uredi = 
  let rec uredi' acc = function  
    |[] -> acc
    | x :: xs -> 
      if x < get 0 xs then x :: acc 
      else uredi' 

    in uredi' [] xs
  


(* -------- 4 -------- *)



(* -------- 5 -------- *)
type priority = 
  |Top
  |Group of int

type status = Staff | Passenger of priority 

type flyer = { status : status ; name : string }

let flyers = [ {status = Staff; name = "Quinn"}
             ; {status = Passenger (Group 0); name = "Xiao"}
             ; {status = Passenger Top; name = "Jaina"}
             ; {status = Passenger (Group 1000); name = "Aleks"}
             ; {status = Passenger (Group 1000); name = "Robin"}
             ; {status = Staff; name = "Alan"}
             ]



             
(* -------- 6 -------- *)

let vkrcavanje = function


(* -------- 7 -------- *)