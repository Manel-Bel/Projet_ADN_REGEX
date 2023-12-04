open Regex_base  

(* version récursive terminale de @*)
let append_rt l1 l2 = 
  List.rev_append (List.rev l1) l2 

(* repeat n w renvoie le mot w concaténé n fois avec lui-même *)
(* reapeat est bien une fonction récursive terminale *)
let repeat n l =
  let rec aux n l acc =
    if n = 0 then acc
    else aux (n-1) l (append_rt l acc)
  in
  aux n l []


(* à revoir *)
(* expr_repeat n e renvoie une expression régulière qui reconnaît les mots
formés de la concaténation de n mots reconnus par e. *)
(* let expr_repeat2 n e = 
  if (n < 1) then Eps
  else
    let rec aux n e = 
      if (n = 1) then e
      else Concat (e, aux (n-1) e)
    in 
    aux n e    
     *)
let expr_repeat n e = 
  if (n < 1) then Eps
  else
    let rec aux n acc = 
      if (n = 1) then acc
      else aux (n-1) (Concat(e,acc))
    in 
    aux n e
;;

(* is_empty e renvoie true ssi le langage reconnu par e ne contient que le mot vide.
  À noter que e n’est pas nécessairement Eps.*)
(* is_empty est bien une fonction récursive terminale *)
let is_empty e =
  let rec aux l =
    match l with
    | [] -> true
    | Concat(a, b)::rest | Alt(a,b)::rest -> aux (a::b::rest)
    | (Base a)::rest -> false
    | Joker::rest -> false
    | Eps::rest -> aux rest
    | (Star a)::rest -> aux (a::rest) 
  in aux [e];
;;


(* null e renvoie true si et seulement si le mot vide est reconnu par e. *)
(* null est une fonction récursive terminale *)
let null e =
  let rec null_aux e fonc =
    match e with 
    | Eps -> fonc true
    | Base a -> fonc false
    | Joker -> fonc false 
    | Concat (a, b) -> 
      null_aux a (fun isNullA -> 
        if isNullA then  null_aux b fonc else fonc false)
    | Alt (a, b) -> 
      null_aux a (fun isNullA -> if isNullA then fonc true else null_aux b fonc )
    | Star a -> fonc true
  in
  null_aux e (fun x -> x) 

(* -------------------------------------------------------------
4.3 Reconnaissance de langages finis
------------------------------------------------------------- *)


(* . Vous pouvez utiliser la fonction union_sorted
: 'a list -> 'a list -> 'a list vue en TD, qui fait l’«union» de deux
ensembles représentés par des listes triées. Vous pouvez aussi utiliser la fonction sort_uniq : 'a list -> 'a list qui renvoie son argument trié et sans
duplicata. Vu le coût de cette fonction, attention à ne pas l’utiliser trop souvent. *)
(* is_empty est bien une fonction récursive terminale *)
let is_finite e =
  let rec aux l = 
    match l with
    | [] -> true
    | Eps::rest | (Base _)::rest | Joker::rest  -> aux rest
    | Concat (a, b)::rest | Alt (a, b)::rest -> aux (a::b::rest)
    | (Star a)::rest -> 
        if (is_empty a) then (aux rest) else false 
  in aux [e]
;;


(* product est bien une fonction récursive terminale *)
let rec product l1 l2 = 
  let rec aux l1 l2 acc =
    match l1 with
    | [] -> acc
    | a::rest -> aux rest l2 (append_rt (List.map (fun souslist -> append_rt a souslist) l2) acc)
  in aux l1 l2 []

(* si e est une expression sur l’ensemble fini de lettres alphabet, alors
enumerate alphabet e renvoie : Some l où l est le langage reconnu par e si ce
langage est fini ; None si ce langage est infini. *)
(* let rec enumerate2 alphabet e =
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
              | Some alpha2 -> Some (alpha1@alpha2) (*à changer*)
             )
        )
    | Star a -> None
;; *)

let rec enumerate alphabet e =
  let rec aux_enumerate e fonc = 
    if not (is_finite e) then fonc None (*Vérif terminaison de l'expr.*)
    else 
      match e with 
      | Eps -> fonc (Some [[]])
      | Base a -> if List.mem a alphabet then fonc (Some [[a]]) else fonc (Some [[]])
      | Joker -> fonc (Some (List.map (fun x -> [x]) alphabet))
      | Concat (a,b) -> 
          aux_enumerate a (fun enumerateA -> 
            match enumerateA with 
            | None -> fonc None
            | Some alphaA -> aux_enumerate b (fun enumerateB ->
              match enumerateB with 
              | None -> fonc None
              | Some alphaB -> fonc (Some (product alphaA alphaB))
              )
            )
      | Alt(a,b) ->
        aux_enumerate a (fun enumerateA -> 
          match enumerateA with 
          | None -> fonc None
          | Some alphaA -> aux_enumerate b (fun enumerateB ->
            match enumerateB with 
            | None -> fonc None
            | Some alphaB -> fonc (Some (append_rt alphaA  alphaB))
            )
          )
      | Star a -> fonc None
   
  in aux_enumerate e (fun x -> x) 
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
(*alphabet est bien une fonction récursive terminale*)
let rec alphabet_expr e = 
  let rec aux l acc =
    match l with 
    | [] -> sort_uniq acc  
    | Eps::rest -> aux rest acc
    | Base a::rest -> aux rest (a::acc)
    | Joker::rest -> aux rest acc
    | Concat (a,b)::rest | Alt (a,b)::rest -> aux (a::b::rest) acc
    | Star a::rest -> aux (a::rest) acc
  in aux [e] []
;;

type answer =
  Infinite | Accept | Reject

(* expressions e et tout mot w, accept_partial e w renvoie
– Infinite si le langage reconnu par e est infini,
– Accept si le langage reconnu par e est fini et contient le mot w,
– Reject si le langage reconnu par e est fini et ne contient pas w.
l'alphabet de e sera l’union des ensembles des lettres apparaissant 
dans e et w. *)
let accept_partial e w = 
  if not (is_finite e) then Infinite
  else 
    let alphabet = union_sorted (alphabet_expr e) w in
    match enumerate alphabet e with
    | None -> Reject
    | Some l -> if List.mem w l then Accept else Reject
;;


