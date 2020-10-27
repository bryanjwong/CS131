(* Problem 1 *)
type ('nonterminal, 'terminal) symbol =
    | N of 'nonterminal
    | T of 'terminal

let convert_grammar (top, rules) = 
    let generate_new_rules nt =
        List.rev (List.fold_left (fun acc (lhs, rhs) ->  if lhs = nt then rhs::acc
            else acc) [] rules)
        in
    (top, generate_new_rules)

(* Problem 2 *)
type ('nonterminal, 'terminal) parse_tree =
    | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
    | Leaf of 'terminal

let parse_tree_leaves tree =
    let rec helper = function
        | [] -> []
        | (Leaf x)::tl -> x::(helper tl)
        | (Node (_,b))::tl -> (helper b)@(helper tl)
    in

    match tree with
        | Leaf x -> [x]
        | Node (_, b) -> helper b

(* Problem 3 *)
let make_matcher (top, rules) =
    
    (* Sub out the nonterminal for the rule *)
    let rec generate_expr sym_rules acceptor frag =
        match sym_rules with 
            | [] -> None
            | hd::tl -> (match evaluate_expr hd acceptor frag with
                            | None -> generate_expr tl acceptor frag
                            | l -> l
            )

    (* Build the possible instruction and check if it matches the fragment *)
    and evaluate_expr expr acceptor frag =
        if expr = [] then acceptor frag
        else match frag with 
            | [] -> None
            | first::rest -> (match expr with
                | [] -> None
                | (T hd)::tl -> if hd = first then evaluate_expr tl acceptor rest else None
                | (N hd)::tl -> generate_expr (rules hd) (evaluate_expr tl acceptor) frag
            )
    in

    let evaluate acceptor fragment =
        evaluate_expr [N top] acceptor fragment
    in
    evaluate

(* Problem 4 *)
let make_parser (top, rules) =
    
    (* Sub out the nonterminal for the rule *)
    let rec generate_expr sym_rules acceptor frag =
        match sym_rules with 
            | [] -> None
            | hd::tl -> (match evaluate_expr hd acceptor frag with
                            | None -> generate_expr tl acceptor frag
                            | Some l -> Some (hd::l)
            )

    (* Build the possible instruction and check if it matches the fragment *)
    and evaluate_expr expr acceptor frag =
        if expr = [] then acceptor frag
        else match frag with 
            | [] -> None
            | first::rest -> (match expr with
                | [] -> None
                | (T hd)::tl -> if hd = first then evaluate_expr tl acceptor rest else None
                | (N hd)::tl -> generate_expr (rules hd) (evaluate_expr tl acceptor) frag
            )
    in

    let check_empty l =
        match l with
            | [] -> Some []
            | _ -> None
    in

    let rec parse_expr expr trace =
        match expr with 
            | [] -> ([], trace)
            | hd::tl -> (match hd with 
                | T term -> (match parse_expr tl trace with
                    | (l, tr) -> ((Leaf term)::l, tr))
                | N nterm -> (match parse_descendant trace with
                    | (l1, tr1) -> (match parse_expr tl tr1 with
                        | (l2, tr2) -> ((Node (nterm, l1))::l2, tr2)
                    )
                )
            )
                
    and parse_descendant trace =
        match trace with 
            | [] -> ([], [])
            | hd::tl -> parse_expr hd tl
    in

    let evaluate fragment =
        match generate_expr [[N top]] check_empty fragment with
            | None -> None
            | Some [] -> None
            | Some (hd::tl) -> (match parse_expr hd tl with
                | ([], _) -> None
                | (x::y, _) -> Some x
            )
    in
    evaluate
