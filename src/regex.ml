open Regex_base

let repeat n l =
  let rec aux n l acc =
    if n = 0 then []
    else repeat (n-1) l@acc
  in
  aux n l l
;;
(* non terminal *)
let rec repeat_not_ter n l = 
  if n = 0 then []
  else l@ (repeat_not_ter (n-1) l) 
;;
let rec expr_repeat n e =
  failwith "À compléter"

let rec is_empty e =
  failwith "À compléter"

let rec null e =
  failwith "À compléter"

let rec is_finite e =
  failwith "À compléter"

let product l1 l2 =
  failwith "À compléter"

let enumerate alphabet e =
  failwith "À compléter"

let rec alphabet_expr e =
  failwith "À compléter"

type answer =
  Infinite | Accept | Reject

let accept_partial e w =
  failwith "À compléter"
