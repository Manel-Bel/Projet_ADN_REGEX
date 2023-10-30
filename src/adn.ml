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
  slices_between [1; 1] [1; 2] [1; 1; 1; 1; 2; 1; 3; 1; 2] = [[1]; []; [2; 1; 3]]
 *)

let cut_genes (dna : dna) : (dna list) = 
  slices_between [A; T; G] [T; A; A] dna;;
;;

(*---------------------------------------------------------------------------*)
(*                          CONSENSUS SEQUENCES                              *)
(*---------------------------------------------------------------------------*)


type 'a consensus = Full of 'a | Partial of 'a * int | No_consensus

(* return (Full a) if all elements of the list are equal to a,
   (Partial (a, n)) if a is the only element of the list with the
   greatest number of occurrences and this number is equal to n,
   No_consensus otherwise. *)

type dict_alpha = {A_: int; C_: int; T_: int; G_: int; WC_: int};;

let consensus (list : 'a list) : 'a consensus =
  let d = {A =  0; C =  0; T =  0; G =  0; WC =  0} in 
  let rec fun_occ list dict_al = 
    match list with
    | [] -> dict_al
    | e::rest-> 
      match e with 
      | A ->  let dict_al = {dict_al with A_ = A_ + 1} 
      | C ->  let dict_al = {dict_al with C_ = C_ + 1} 
      | G ->  let dict_al = {dict_al with G_ = G_ + 1} 
      | T ->  let dict_al = {dict_al with G_ = G_ + 1}
      | WC -> let dict_al = {dict_al with WC_ = WC_ + 1}
    in fun_occ rest dict_al 
  in 
  let nv_d = fun_occ list d in 
  let couple_max = [(A, nv_d.A_); (None , 0)] in 

  if nv_d.C_ > snd (List.hd couple_max) then 
    begin 
      let couple_max = [(C, nv_d.C_); (None , 0)] in 
    end 
    

    [(A =  0;) (C =  0); (T =  0); G =  0; WC =  0]
  
List.filter (fun (b, occ) -> if occ > (snd (List.hd couple_max)) then couple_max = [ (b, occ); (None , 0)]
else  if occ > (snd (List.hd couple_max))
  )
  (* failwith "À compléter" *)


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
  failwith "À compléter"

(*
 consensus_sequence [[1; 1; 1; 1];
                     [1; 1; 1; 2];
                     [1; 1; 2; 2];
                     [1; 2; 2; 2]]
 = [Full 1; Partial (1, 3); No_consensus; Partial (2, 3)]

 consensus_sequence [[]; []; []] = []
 *)

