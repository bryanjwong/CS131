type test_nonterminals = 
    | Top | Animal | Mammal | Plant 
let test_grammar = 
    (Top,
    function
        | Top -> [[N Animal];
                  [N Plant];
                  [N Animal; T"eats"; N Plant]]
        | Animal -> [[N Mammal]]
        | Mammal -> [[T"Goat"]]
        | Plant -> [[T"Rose"]])

let make_matcher_test = ((make_matcher test_grammar accept_all
     ["Goat";"eats";"Rose"])
  = Some ["eats";"Rose"])

 let make_parser_test = ((match make_parser test_grammar ["Goat";"eats";"Rose"] with
    | Some tree -> parse_tree_leaves tree = ["Goat";"eats";"Rose"]
    | _ -> false))