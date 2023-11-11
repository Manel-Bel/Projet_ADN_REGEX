type base = A | C | G | T | WC (* wildcard *)

type dna = base list


(*---------------------------------------------------------------------------*)
(*                               ECHAUFFEMENT                                *)
(*---------------------------------------------------------------------------*)


let string_of_base (b : base) : string =
  match b with
    A -> "A"
  | C -> "C"
  | G -> "G"
  | T -> "T"
  | WC -> "."


(* explode a string into a char list *)
let explode (str : string) : char list =
  let rec aux chaine index acc =
    if index < 0 then
      acc 
    else aux chaine (index - 1) (chaine.[index] :: acc) 
  (* failwith "À compléter" *)
  in
  aux str ((String.length str) -1) []
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

let dna_of_string (s : string) : base list =
  List.map base_of_char (explode s);;



let string_of_dna (seq : dna) : string =
  let char_of_base (b : base) : char =
    match b with 
    | A -> 'A' 
    | C -> 'C'
    | G -> 'G'
    | T -> 'T'
    | WC -> '.'
  in 
  let rec aux liste =
    match liste with 
    | [] -> ""
    | x::rest -> (Char.escaped x)^(aux rest) 
  in 
  aux (List.map char_of_base seq)

 


(*---------------------------------------------------------------------------*)
(*                                   SLICES                                  *)
(*---------------------------------------------------------------------------*)
(*
   Une {\em tranche} de $l = \langle x_1,\dots x_n\rangle$ est une sous-liste
   de $l$ de la forme $\langle x_i, \dots x_{j}$, o\`u $1 \leq i \leq j\leq n$.
 *)


(* if list = pre@suf, return Some suf. otherwise, return None *)
let rec cut_prefix (slice : 'a list) (list : 'a list) : 'a list option =
  match (slice,list) with 
  | ([], l ) -> Some l
  | (_ , []) -> None
  | (l1::reste1, l2::reste2) -> 
    if not (l1 = l2) then None 
    else cut_prefix reste1 reste2;;

(*
  cut_prefix [1; 2; 3] [1; 2; 3; 4] = Some [4]
  cut_prefix [1; 2; 3; 4] [1; 2; 3; 4] = Some []
  cut_prefix [1; 2; 0] [1; 2; 3; 4] = None
 *)


(* return the prefix and the suffix of the first occurrence of a slice,
   or None if this occurrence does not exist.
*)
   let first_occ (slice : 'a list) (list : 'a list)
   : ('a list * 'a list) option =
   let rec aux pattern before list =
     match (list , pattern) with
     | ([], []) -> Some ([],[])
     | ([], _) -> None
     | (x::rest, _) -> 
         match cut_prefix pattern list with
         | None -> aux pattern (x::before) rest
         | Some suffixe -> Some (List.rev before, suffixe)
   in aux slice [] list

(*
  first_occ [1; 2] [1; 1; 1; 2; 3; 4; 1; 2] = Some ([1; 1], [3; 4; 1; 2])
  first_occ [1; 1] [1; 1; 1; 2; 3; 4; 1; 2] = Some ([], [1; 2; 3; 4; 1; 2])
  first_occ [1; 3] [1; 1; 1; 2; 3; 4; 1; 2] = None
 *)

let rec slices_between
  (start : 'a list) (stop : 'a list) (list : 'a list) : 'a list list =
  match list with
  | [] -> []
  | _ -> 
    match first_occ start list with 
    | None -> []
    | Some (before_start, suffix_start) -> 
      match (first_occ stop suffix_start) with 
      | None -> []
      | Some (before_stop, suffix_stop) -> before_stop :: (slices_between start stop suffix_stop)
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


let max1_max2 (list : int list) =
  let rec aux m1 m2 list =
    match list with 
    | [] -> (!m1, !m2)
    | freq1::rest -> 
        if (freq1 > !m2) then 
          m2 := freq1;
        if (freq1 > !m1) then (m1 := freq1);
        aux m1 m2 rest
  in 
  aux (ref 0) (ref 0) list
          
;;

let freq list list_doublon = 
  let rec aux elm l acc =
    match l with
    | [] -> acc
    | e::rest -> if(e = elm) then (aux elm rest (1+acc)) else (aux elm rest acc)
  in 
  List.map (fun e -> (aux e list_doublon 0)) list
;;

let get_indice_elm l1 l2 elm =
  let rec aux l1 l2 i =
    match (l1, l2) with
    | (e1::rest1,e2::rest2) -> if (i == e2) then Some e1 else aux rest1 rest2 i
    | (_,_) -> None 
  in 
  aux l1 l2 elm
;;

(* liste l, calcule le consensus de ses valeurs, défini comme :
– Full b si tous les éléments de l sont égaux à b,
– Partial (b, n) si b est l’unique valeur apparaissant le plus grand nombre de fois dans l 
– No_consensus :: autres cas. *)
let consensus (list : 'a list) : 'a consensus =
  match list with 
  | [] -> No_consensus
  | e::reste -> 
    let set_list = union_sorted list [] in 
    let freq_list = freq set_list list in 
    let (m1,m2) = max1_max2 freq_list in 


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
  failwith "À compléter";;

(*
 consensus_sequence [[1; 1; 1; 1];
                     [1; 1; 1; 2];
                     [1; 1; 2; 2];
                     [1; 2; 2; 2]]
 = [Full 1; Partial (1, 3); No_consensus; Partial (2, 3)]

 consensus_sequence [[]; []; []] = []
 *)

