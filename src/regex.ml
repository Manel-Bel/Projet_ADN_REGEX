open Regex_base

let repeat n l =
  let rec aux n l acc =
    if n = 0 then []
    else aux (n-1) l@acc
  in
  aux n l l
;;

(* non terminal *)
let rec repeat_not_ter n l = 
  if n = 0 then []
  else l@ (repeat_not_ter (n-1) l) 
;;

(* à revoir *)
let expr_repeat n e = 
  if (n < 1) then Esp
  else
    let rec aux n e = 
      if (n = 1) then e;
      else Concat(e, aux (n-1) e)
;;
(* telle que is_empty e renvoie true si et seulement si le langage reconnu par e ne *)

let rec is_empty e =
  match e with 
  | Eps  -> true 
  | Base a -> false 
  | Concat (a , b) -> (is_empty a ) && (is_empty b)
  | Alt (a,b) -> (is_empty a ) || (is_empty b)
  | Star a -> is_empty a
  | _ -> false
;;

let rec null e =
  match e with
  | Eps | Star Eps -> true
  | Base a -> false
  | Concat (a, b) -> (null a) && (null b)
  | Alt (a, b) -> (null a) || (null b)
  | Star a -> null a                        
  | _ -> false
;;

let rec is_finite e =
  match e with 
  | Eps | Base _ | Joker  -> true
  | Concat (a, b) | Alt (a, b) -> ((is_finite a) && (is_finite b))
  | Star a -> 
    if (is_empty a) then true else false 
;;

(* product l1 l2 renvoie l’ensemble des mots formés de la concaténation
d’un mot de l1 et d’un mot de l2 *)
let rec product l1 l2 =
  match l1 with
  | [] -> []
  | a::rest -> (List.map (fun souslist -> a@souslist) l2)@(product rest l2)
;;


(* si e est une expression sur l’ensemble fini de lettres alphabet, alors
enumerate alphabet e renvoie : Some l où l est le langage reconnu par e si ce
langage est fini ; None si ce langage est infini. *)
let rec enumerate alphabet e =
  if not (is_finite e) then None
  else 
    match e with 
    | Eps -> Some [[]]
    | Base a -> if List.mem a alphabet then Some [[a]] else Some [[]]
    | Joker -> Some (List.map (fun x -> [x]) alphabet)
    | Concat (a,b) -> 
        (match (enumerate alphabet a) with
         | None -> None
         | Some alpha1 ->
             (match (enumerate alphabet b) with 
              | None -> None
              | Some alpha2 -> Some (product alpha1 alpha2)
             )
        )
    | Alt (a,b) -> 
        (match (enumerate alphabet a) with
         | None -> None
         | Some alpha1 ->
             (match (enumerate alphabet b) with 
              | None -> None
              | Some alpha2 -> Some (alpha1@alpha2)
             )
        )
    | Star a -> None
;;

(* alphabet_expr e renvoie l’ensemble (la liste triée sans duplicata) des
lettres apparaissant dans e *)
let rec alphabet_expr e =
  (* match e with 
  | Eps ->  []
  | Base a -> [a]
  | Joker -> []
  | Concat (a,b) ->  union_sorted (alphabet_expr a) (alphabet_expr b)
  | Alt (a,b) ->   union_sorted (alphabet_expr a)  (alphabet_expr b)
  | Star a -> (alphabet_expr a) *)
  failwith "À compléter"
;;

type answer =
  Infinite | Accept | Reject

let rec accept_partial e w =
  (* if not (is_finite e) then Infinite
  else 
    let alpha = union_sorted (alphabet_expr e) w in
    let rec contient e w =
      match e with
      | Eps -> if (w = []) then true else false 
      | Base a -> if (List.mem a w )then true else false
      | Concat (a, b) -> (contient a w) && (contient b w)
      | Alt (a, b) -> (contient a w) || (contient b w)
      | Star a -> contient a w                     
      | _ -> false
    in 
    if contient e alpha then Accept else Reject  *)
    failwith "À compléter"
;;
