open Regex_base

let repeat n l =
  let rec aux n l acc =
    if n = 0 then []
    else aux (n-1) l l@acc
  in
  aux n l l
;;

(* à revoir *)

(* expr_repeat n e renvoie une expression régulière qui reconnaît les mots
formés de la concaténation de n mots reconnus par e. *)
let expr_repeat n e = 
  if (n < 1) then Eps
  else
    let rec aux n e = 
      if (n = 1) then e
      else Concat (e, aux (n-1) e)
    in 
    aux n e 
;;

let rec is_empty' l =
  match l with
  | [] -> true
  | Concat(a, b)::rest | Alt(a,b)::rest -> is_empty' (a::b::rest)
  | (Base a)::rest -> false
  | Joker::rest -> false
  | Eps::rest -> is_empty' rest
  | (Star a)::rest -> is_empty' (a::rest) 

  (* is_empty e renvoie true ssi le langage reconnu par e ne contient que le mot vide.
   À noter que e n’est pas nécessairement Eps.*)
let is_empty e = is_empty' [e];
;;




(* null e renvoie true si et seulement si le mot vide est reconnu par e. *)
let rec null e =
  match e with
  | Eps -> true
  | Base a -> false
  | Joker -> false 
  | Concat (a, b) -> (null a) && (null b)
  | Alt (a, b) -> (null a) || (null b)
  | Star a -> true
  (* il nous a pas encore dit *)
  (* | _ -> false this match case is unused. *)
;;

(* -------------------------------------------------------------
4.3 Reconnaissance de langages finis
------------------------------------------------------------- *)


(* . Vous pouvez utiliser la fonction union_sorted
: 'a list -> 'a list -> 'a list vue en TD, qui fait l’«union» de deux
ensembles représentés par des listes triées. Vous pouvez aussi utiliser la fonction sort_uniq : 'a list -> 'a list qui renvoie son argument trié et sans
duplicata. Vu le coût de cette fonction, attention à ne pas l’utiliser trop souvent. *)


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
  if not (is_finite e) then None (*Vérif terminaison de l'expr.*)
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

(* union_sorted renvoie la liste triée sans duplicata *)
let union_sorted l1 l2 =
  let rec aux l1 l2 acc =
    match l1,l2 with 
    | [], [] -> List.rev acc
    | [], _ -> List.rev_append acc l2
    | _, [] -> List.rev_append acc l1
    | x::l1', y::l2' -> 
        if (x = y) then aux l1' l2' (x::acc)
        else if (x < y) then aux l1' l2 (x::acc)
        else aux l1 l2' (y::acc) 
  in aux l1 l2 []
;; 
  
(* alphabet_expr e renvoie l’ensemble (la liste triée sans duplicata) des
lettres apparaissant dans e *)
let rec alphabet_expr e = 
  match e with 
  | Eps ->  []
  | Base a -> [a]
  | Joker -> []
  | Concat (a,b) | Alt (a,b) ->  union_sorted (alphabet_expr a) (alphabet_expr b)
  | Star a -> (alphabet_expr a)
;;

type answer =
  Infinite | Accept | Reject

(* expressions e et tout mot w, accept_partial e w renvoie
– Infinite si le langage reconnu par e est infini,
– Accept si le langage reconnu par e est fini et contient le mot w,
– Reject si le langage reconnu par e est fini et ne contient pas w.
l'alphabet de e sera l’union des ensembles des lettres apparaissant 
dans e et w. *)
let rec accept_partial e w = 
  if not (is_finite e) then Infinite
  else 
    let alphabet = union_sorted (alphabet_expr e) w in
    match enumerate alphabet e with
    | None -> Reject
    | Some l -> if List.mem w l then Accept else Reject
;;


