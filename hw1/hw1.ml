(* Problem 1 *)
let rec subset a b =
    match a with
    | [] -> true
    | h::t -> List.exists (fun x -> x = h) b && subset t b

(* Problem 2 *)
let equal_sets a b = subset a b && subset b a

(* Problem 3 *)
let set_union a b =
    let rec add_to_union x y =
        match x with
        | [] -> y
        | h::t -> if not(List.exists (fun z -> z = h) y)
                      then add_to_union t (h::y)
                      else add_to_union t y in
    add_to_union a (add_to_union b [])

(* Problem 4 *)
let set_intersection a b =
    let rec add_to_intersection x y z =
        match x with
        | [] -> z
        | h::t -> if List.exists (fun e -> e = h) y && not(List.exists (fun e -> e = h) z)
                  then add_to_intersection t y (h::z)
                  else add_to_intersection t y z in
    add_to_intersection a b []

(* Problem 5 *)
let rec set_diff a b =
    match a with
    | [] -> []
    | h::t -> if List.exists (fun x -> x = h) b then set_diff t b
              else h::(set_diff t b)

(* Problem 6 *)
let rec computed_fixed_point eq f x =
    let res = f x in
    if eq x res then x
    else computed_fixed_point eq f res

(* Problem 7*)
type ('nonterminal, 'terminal) symbol =
    | N of 'nonterminal
    | T of 'terminal

let filter_reachable (top, gram) =
    let extract_nts nt =
        List.fold_left (fun acc (a, b) -> if List.mem a nt
            then set_union acc (List.filter_map (fun x -> match x with | N y -> Some y | T _ -> None) b)
            else acc) nt gram
        in

    let rec find_valid_nts nt =
        let new_nts = set_diff (extract_nts nt) nt in
        match new_nts with
            | [] -> nt
            | _ -> find_valid_nts (set_union nt new_nts)
        in

    let rec find_valid_rules nt rules =
        match rules with
            | [] -> []
            | (a,b)::t -> if List.mem a nt then (a,b)::(find_valid_rules nt t)
                          else find_valid_rules nt t
        in

    top, find_valid_rules (find_valid_nts [top]) gram
