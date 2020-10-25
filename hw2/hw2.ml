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




