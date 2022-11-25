type state = Q0 | Q1 | Q2;;

let data : state list = Q0::Q1::Q2::[] ;; 

let explode_string s = List.init (String.length s) (String.get s);;

let transiton : state * char -> state = function 
  | (Q0, '1') -> Q1
  | (Q0, '0') -> Q0
  | (Q1, '1') -> Q1
  | (Q1, '0') -> Q1
  | (_, _) -> Q0 ;; 
  
let final : state -> bool = function | Q1 -> true | _ -> false ;;

let rec compute(start : state)(lst : char list) : state = 
  match lst with 
  | [] -> start
  | c::cs -> compute (transiton(start, c)) cs in
 
  let accept(input : string) : bool = 
    final(compute(Q0)(explode_string input)) in 

    accept("01") ;;

(* dfa minimzation next *)

let rec finals : state list -> state list= function 
 | [] -> []
 | x :: xs -> if (transiton(x, '1') == true) then x :: finals xs else finals xs