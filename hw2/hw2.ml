(* Problem 1 *)
type ('nonterminal, 'terminal) symbol =
    | N of 'nonterminal
    | T of 'terminal

let convert_grammar (top, rules) = 
    let generate_new_rules nt =
        List.fold_left (fun acc (lhs, rhs) ->  if lhs = nt then acc@[rhs]
            else acc) [] rules
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

    let rec check_prefix a b =
        if a = [] then Some a
        else if b = [] then None
        else if (List.hd a) != (List.hd b) then None
        else check_prefix (List.tl a) (List.tl b)
    in

    (* Sub out the nonterminal for the rule *)
    let rec generate_expr sym_rules tail build frag =
        match sym_rules with 
            | [] -> None
            | hd::tl -> match (evaluate_expr (hd@tail) build frag) with
                            | Some x -> Some x
                            | None -> generate_expr tl tail build frag

    (* Build the possible instruction and check if it's a prefix *)
    and evaluate_expr expr build frag =
        match expr with
            | [] -> check_prefix build frag
            | (T hd)::tl -> evaluate_expr tl (build@[hd]) frag
            | (N hd)::tl ->
                let hd_rules = rules hd in
                generate_expr hd_rules tl build frag
    in

    evaluate_expr [N top] []


