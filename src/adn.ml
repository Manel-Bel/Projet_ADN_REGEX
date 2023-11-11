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

(* return (Full a) if all elements of the list are equal to a,
   (Partial (a, n)) if a is the only element of the list with the
   greatest number of occurrences and this number is equal to n,
   No_consensus otherwise. *)
(* let occurrence list = 
  let tab = Array.make 5 0 in 
  let rec aux = function 
  | [] -> ()
  | e::rest ->
    match e with 
    | A -> tab.(0) <- tab.(0) + 1
    | C -> tab.(1) <- tab.(1) + 1
    | G -> tab.(2) <- tab.(2) + 1
    | T -> tab.(3) <- tab.(3) + 1
    | WC -> tab.(4) <- tab.(4) + 1
    aux rest
  in 
  aux list;
  tab
;; *)

(* let max1_max2 tab =
  Array.sorted (fun x y -> compare y x) tab;
  (tab.(0), tab.(1))
;;

let get_elem tab occ = 
  match occ with 
  | tab.(0) -> A
  | tab.(1) -> C
  | tab.(2) -> G
  | tab.(3) -> T
  | _ -> WC
;; *)


let consensus (list : 'a list) : 'a consensus =
   failwith "À compléter"
  (* match list with
  | [] -> No_consensus
  | _ -> 
    let tab1 = occurrence list in 
    let (max1, max2) = max1_max2 tab in 
    if max1 = max2 then No_consensus
    else 
      let e1 =  match max1 with 
      | tab.(0) -> A
      | tab.(1) -> C
      | tab.(2) -> G
      | tab.(3) -> T
      | _ -> WC
    in 
      if max2 = 0 then Full e1
      else 
        Partial(e1, max1) *)
      ;;

      (* let consensus (list : 'a list) : 'a consensus =
      match list with
      | [] -> No_consensus
      | _ -> 
          let tab = occurrence list in 
          let (max1, max2) = max1_max2 tab in 
          if max1 = max2 then No_consensus
          else 
            let e1 =  match max1 with 
              | tab.(0) -> A
              | tab.(1) -> C
              | tab.(2) -> G
              | tab.(3) -> T
              | _ -> WC
            in 
            if max2 = 0 then Full e1
            else 
              Partial(e1, max1)
    ;; *)

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

