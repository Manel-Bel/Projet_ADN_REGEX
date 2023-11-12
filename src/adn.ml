type base = A | C | G | T | WC (* wildcard *)

type dna = base list


(*---------------------------------------------------------------------------*)
(*                               ECHAUFFEMENT                                *)
(*---------------------------------------------------------------------------*)


let string_of_base (b : base) : string =
  match b with
  | A -> "A"
  | C -> "C"
  | G -> "G"
  | T -> "T"
  | WC -> "."
;;

(* explode : convertit une chaîne de caractères en une liste de caractères *)
let explode (str : string) : char list =
  let rec aux chaine index acc = 
    if index < 0 then (*on s'arrete si on est au debut de la chaine *)
      acc (*la liste de char*)
    else aux chaine (index - 1) (chaine.[index] :: acc) (*on dec l'indice *)
  in
  aux str ((String.length str) -1) []  (* appel avec la chaine (indice du dernier char) acc  *)
;;


(* conversions *)
let base_of_char (c : char) : base =
  match c with 
  | 'A' -> A 
  | 'C' -> C
  | 'G' -> G
  | 'T' -> T
  | _ -> WC
;; 

(*conversion d'un caractère en base nucléique*)
let dna_of_string (s : string) : base list =
  List.map base_of_char (explode s);; (*application de base_of_char sur la liste de caractère s*)


(*conversion d'une chaîne de caractères en un brin d’adn*)
let string_of_dna (seq : dna) : string =
  (*conversion en char d'un base*)
  let char_of_base (b : base) : char = 
    match b with 
    | A -> 'A' 
    | C -> 'C'
    | G -> 'G'
    | T -> 'T'
    | WC -> '.'
  in 
  let rec aux liste acc = (*concatenation de chaque char*) 
    match liste with 
    | [] -> acc
    | x::rest -> (aux rest (acc^(Char.escaped x))) (*Char.escaped = convertit un char en String*)
  in 
  aux (List.map char_of_base seq) ""
;;
 

(*---------------------------------------------------------------------------*)
(*                                   SLICES                                  *)
(*---------------------------------------------------------------------------*)
(*
   Une {\em tranche} de $l = \langle x_1,\dots x_n\rangle$ est une sous-liste
   de $l$ de la forme $\langle x_i, \dots x_{j}$, o\`u $1 \leq i \leq j\leq n$.
 *)


(* prend en entrée une liste pre et une liste l, et renvoie :
  – None, si la liste pre n’est pas un préfixe de l.
  – Some(suf), si l = pre @ suf.*)
let rec cut_prefix (slice : 'a list) (list : 'a list) : 'a list option =
  match (slice,list) with 
  | ([], l ) -> Some l (*si pre vide -> la list*)
  | (_, []) -> None (*si la liste est vide --> None*)
  | (l1::reste1, l2::reste2) -> 
    if not (l1 = l2) then None (*si le 1er elm de slice <> du 1er elm de List --> slice n'est pas prefix*)
    else cut_prefix reste1 reste2;; (*sinon on verif pour le reste *)

(*
  cut_prefix [1; 2; 3] [1; 2; 3; 4] = Some [4]
  cut_prefix [1; 2; 3; 4] [1; 2; 3; 4] = Some []
  cut_prefix [1; 2; 0] [1; 2; 3; 4] = None
 *)


(* retouner le  prefix et suffix de la 1ere occurrence de slice,
   ou None si cette occurrence n'existe pas .
   --> l = before @ slice @ after
*)
let first_occ (slice : 'a list) (list : 'a list) : ('a list * 'a list) option =
  let rec aux pattern before list =
    match (list , pattern) with
    | ([], []) -> Some ([],[]) (*si la liste est vide et le motif de la rech vide ->  Some ([],[]) *)
    | ([], _) -> None (*si  liste vide or que le motif n'est pas vide -> None*)
    | (x::rest, _) -> 
        match cut_prefix pattern list with (*sinon on check si pattern est bien le prefix *)
        | None -> aux pattern (x::before) rest (*si le suffix = None -> on concat le elem avant dans before *)
        | Some suffixe -> Some (List.rev before, suffixe) 
  in aux slice [] list

(*
  first_occ [1; 2] [1; 1; 1; 2; 3; 4; 1; 2] = Some ([1; 1], [3; 4; 1; 2])
  first_occ [1; 1] [1; 1; 1; 2; 3; 4; 1; 2] = Some ([], [1; 2; 3; 4; 1; 2])
  first_occ [1; 3] [1; 1; 1; 2; 3; 4; 1; 2] = None
 *)

(* renvoie la liste de toutes les sous_liste encadrées par start et stop *)
let slices_between
  (start : 'a list) (stop : 'a list) (list : 'a list) : 'a list list =
  let rec aux start stop list acc = 
    match list with
    | [] -> List.rev acc (*acc = accumulateur de ]start;stop[ *)
    | _ -> 
      match first_occ start list with 
      | None -> List.rev acc (*S'il y n'y pas/plus de prefixe dans la liste*)
      | Some (before_start, suffix_start) -> 
        match (first_occ stop suffix_start) with 
        | None -> List.rev acc (*S'il y n'y pas/plus de suffixe dans la liste*)
        (*On rappelle aux pour trouver les autres occurences*)
        | Some (before_stop, suffix_stop) -> aux start stop suffix_stop (before_stop::acc)   
  in aux start stop list []
  ;;

(*
  slices_between [1; 1] [1; 2] [1; 1; 1; 1; 2; 1; 3; 1; 2] = [[1]]
 *)

let cut_genes (dna : dna) : (dna list) = 
  slices_between [A; T; G] [T; A; A] dna;;
;;

(*---------------------------------------------------------------------------*)
(*                          CONSENSUS SEQUENCES                              *)
(*---------------------------------------------------------------------------*)


type 'a consensus = Full of 'a | Partial of 'a * int | No_consensus;;

let remove_duplicates (liste : 'a list) : 'a list =
  let rec aux liste1 acc = 
    match liste1 with
    | [] -> acc (*acc contient la nouvelle liste sans doublons*)
    | e::restes -> 
      if (List.mem e acc) then (aux restes acc) (*si on a deja prit e alors on change pas acc*)
      else aux restes (e::acc)
  in 
  aux liste []
;;

let max1_max2 (list_f : int list) (list_e : 'a list) =
  let rec aux e1 m1 e2 m2 l_f l_e=
    match (l_f,l_e) with 
    | ([],[]) -> ((!e1,!m1), (!e2,!m2))
    | (freq::rest_f,e::rest_e) -> 
        if (freq >= !m1) then
          begin 
            m2 := !m1;
            e2 := !e1;
            m1 := freq;
            e1 := Some e;
          end
        else 
        if freq > !m2 then 
          begin 
            e2 := Some e;
            m2 := freq;
          end;
        aux e1 m1 e2 m2 rest_f rest_e
    | (_,_) -> ((None,0),(None,0))
  in 
  aux (ref None) (ref 0) (ref None) (ref 0 ) list_f list_e
          
;;

let freq list list_doublon = 
  let rec aux elm l acc =
    match l with
    | [] -> acc
    | e::rest -> if(e = elm) then (aux elm rest (1+acc)) else (aux elm rest acc)
  in 
  List.map (fun e -> (aux e list_doublon 0)) list
;;


(* liste l, calcule le consensus de ses valeurs, défini comme :
– Full b si tous les éléments de l sont égaux à b,
– Partial (b, n) si b est l’unique valeur apparaissant le plus grand nombre de fois dans l 
– No_consensus :: autres cas. *)
let consensus (list : 'a list) : 'a consensus =
  match list with 
  | [] -> No_consensus
  | e::reste -> 
    let set_list = remove_duplicates list in 
    let freq_list = freq set_list list in 
    let ((e1, m1),(e2,m2)) = max1_max2 freq_list set_list in 
    match (e1,e2) with 
    | (Some e , None) -> Full e (*e1 seul dans set_list*)
    | (Some x, Some y) -> if (m1 = m2) then No_consensus else Partial (x,m1)
    | _ -> No_consensus
;;

(*
   consensus [1; 1; 1; 1] = Full 1
   consensus [1; 1; 1; 2] = Partial (1, 3)
   consensus [1; 1; 2; 2] = No_consensus
 *)

(* return the consensus sequence of a list of sequences : for each position
   in the elements of ll, compute the consensus  of the set of values at this
   position  in the sequences. the lists must be of same length. if all lists
   are empty, return the empty sequence.
 *)

let consensus_sequence (ll : 'a list list) : 'a consensus list =
  List.map (fun sous_l -> consensus sous_l) ll ;;
   

  (*
 consensus_sequence [[1; 1; 1; 1];
                     [1; 1; 1; 2];
                     [1; 1; 2; 2];
                     [1; 2; 2; 2]]
 = [Full 1; Partial (1, 3); No_consensus; Partial (2, 3)]

 consensus_sequence [[]; []; []] = []
 *)