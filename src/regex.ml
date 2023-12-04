open Regex_base  

(* version récursive terminale de @*)
let append_rt l1 l2 = 
  List.rev_append (List.rev l1) l2 

(* renvoie le mot w concaténé n fois avec lui-même *)
let repeat n l =
  let rec aux n l (acc : 'a list) =
    if n = 0 then acc (* Fin de la récursion, retour du resultat *)
    else aux (n-1) l (append_rt l acc) (* concatene le resultat précédent (acc) à w (w=l)*)
  in
  aux n l []


(* renvoie une expression régulière qui reconnaît les mots
formés de la concaténation de n mots reconnus par e. *)
let expr_repeat n e = 
  if (n < 1) then Eps
  else
    let rec aux n (acc : 'a expr) = 
      if (n = 1) then acc (* Fin de la récursion, retour du resultat *)
      else aux (n-1) (Concat(e,acc))  (* concatene le resultat précédent (acc) à w (w=l)*)
    in 
    aux n e
;;

(* is_empty e renvoie true ssi le langage reconnu par e ne contient que le mot vide.
  À noter que e n’est pas nécessairement Eps.*)
let is_empty e =
  let rec aux (l : 'a expr list) =
    match l with 
    | [] -> true (* Plus d'expression en file d'attente *)
    | Concat(a, b)::rest | Alt(a,b)::rest -> aux (a::b::rest) (*Rajout dans la liste des expr à match*)
    | (Base a)::rest -> false
    | Joker::rest -> false
    | Eps::rest -> aux rest
    | (Star a)::rest -> aux (a::rest) (*Rajout dans la liste des expr à match*)
  in aux [e];
;;


(* null e renvoie true si et seulement si le mot vide est reconnu par e. *)
let null e =
  (*fonc aux qui prend en par l'expression e et une continuation 'fonc' *)
  let rec null_aux e fonc =
    match e with 
    | Eps -> fonc true (*si Eps, appel à la continuation avec vrai*)
    | Base a -> fonc false (*si Eps, appel à la continuation avec faux*)
    | Joker -> fonc false 
    | Concat (a, b) -> 
      null_aux a (fun isNullA -> (*verif si a est null*)
        if isNullA then null_aux b fonc 
        else fonc false (*a etait null donc on appelle la continuation avec faux*))
    | Alt (a, b) -> 
      null_aux a (fun isNullA -> 
        if isNullA then fonc true (*a contient bien Eps donc appel directement à la continuation avec vrai*)
        else null_aux b fonc (*sinon on teste avec b *) )
    | Star a -> fonc true
  in
  null_aux e (fun x -> x) (*appel initial avec la fonc identité  pour bien obtenir le resultat final*)

(* -------------------------------------------------------------
4.3 Reconnaissance de langages finis
------------------------------------------------------------- *)


(* . Vous pouvez utiliser la fonction union_sorted
: 'a list -> 'a list -> 'a list vue en TD, qui fait l’«union» de deux
ensembles représentés par des listes triées. Vous pouvez aussi utiliser la fonction sort_uniq : 'a list -> 'a list qui renvoie son argument trié et sans
duplicata. Vu le coût de cette fonction, attention à ne pas l’utiliser trop souvent. *)
let is_finite e =
  let rec aux l = 
    match l with
    | [] -> true (* Plus d'expression en file d'attente *)
    | Eps::rest | (Base _)::rest | Joker::rest  -> aux rest
    | Concat (a, b)::rest | Alt (a, b)::rest -> aux (a::b::rest) (*Rajout dans la liste des expr à match*)
    | (Star a)::rest -> 
        if (is_empty a) then (aux rest) else false 
  in aux [e]
;;

(* renvoie l’ensemble des mots formés de la concaténation
d’un mot de l1 et d’un mot de l2 *)
let rec product l1 l2 = 
  let rec aux l1 l2 acc =
    match l1 with
    | [] -> acc
    | a::rest -> 
      let tmp = (List.map (fun souslist -> append_rt a souslist) l2) in (*concatene a (elm de l1) à tous element de l2*)
        aux rest l2 (append_rt tmp acc) (*on ajoute le res dans l'acc et on rappelle aux sur la suite de l1*)
  in aux l1 l2 []



(* Version non récursive terminale d'enumerate
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
              | Some alpha2 -> Some (alpha1@alpha2) (*à changer*)
             )
        )
    | Star a -> None
;; *)

(* si e est une expression sur l’ensemble fini de lettres alphabet, alors
enumerate alphabet e renvoie : Some l où l est le langage reconnu par e si ce
langage est fini ; None si ce langage est infini. *)
 let rec enumerate alphabet e =
  (*fonc aux qui prend en par l'expression e et une continuation 'fonc' *)
  let rec aux_enumerate e fonc = 
    if not (is_finite e) then fonc None 
    else 
      match e with 
      | Eps -> fonc (Some [[]]) (*si Eps, appel à la continuation avec Some de liste vide *)
      | Base a -> 
        if List.mem a alphabet then fonc (Some [[a]]) 
          (*si a est dans l'alphabet alors appel à la continuat° de Some d'une liste avec l'elem a*)
        else fonc (Some [[]])
      | Joker -> fonc (Some (List.map (fun x -> [x]) alphabet))
      | Concat (a,b) -> 
        (*recuperation de l'alpahbet de a *)
          aux_enumerate a (fun enumerateA -> 
            match enumerateA with 
            | None -> fonc None (*si l'alphabet de a est None alors appel direct à la continuation *)
            | Some alphaA -> aux_enumerate b (fun enumerateB ->
              (*alphabet de A existe on essaye de recup l'alphabet de B *)
              match enumerateB with 
              | None -> fonc None 
              | Some alphaB -> 
                (*alphaA et alphaB existent donc appel à la continuat° avec le produit des deux  *)
                fonc (Some (product alphaA alphaB)) 
              )
            )
      | Alt(a,b) ->
        aux_enumerate a (fun enumerateA -> 
          match enumerateA with 
          | None -> fonc None
          | Some alphaA -> aux_enumerate b (fun enumerateB ->
            match enumerateB with 
            | None -> fonc None
            | Some alphaB -> 
              (*alphaA et alphaB existent donc appel à la continuat° avec la concat des deux*)
              fonc (Some (append_rt alphaA  alphaB))
            )
          )
      | Star a -> fonc None
   
  in aux_enumerate e (fun x -> x) (*appel initial avec la fonc identité pour bien obtenir le resultat final*)
;; 



(* union_sorted renvoie la liste triée sans duplicata 
version terminale*)
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
  let rec aux (l : 'a expr list) (acc : 'a list) =
    match l with 
    | [] -> sort_uniq acc (* Plus d'expression en file d'attente *) 
    | Eps::rest -> aux rest acc
    | Base a::rest -> aux rest (a::acc)
    | Joker::rest -> aux rest acc
    | Concat (a,b)::rest | Alt (a,b)::rest -> aux (a::b::rest) acc (*Rajout dans la liste des expr à match*)
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
    (*Récupération de tout l'alphabet*)
    let alphabet = union_sorted (alphabet_expr e) w in 
    match enumerate alphabet e with 
    | None -> Reject (* N'est pas censé ê ici*)
    | Some l -> if List.mem w l then Accept else Reject (*w n'appartient pas à e*)
;;


