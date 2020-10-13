let my_subset_test0 = subset [] []
let my_subset_test1 = subset [] [1]
let my_subset_test2 = subset [] [1;2;3]
let my_subset_test3 = subset [1] [1]
let my_subset_test4 = subset [1] [1;2;3]
let my_subset_test5 = not (subset [1] [])
let my_subset_test6 = not (subset [1] [2])
let my_subset_test7 = subset [1;2] [2;1]
let my_subset_test8 = subset [1;2] [1;2;3]
let my_subset_test9 = subset [1;2;1] [1;2]
let my_subset_test10 = subset [1;2;1] [1;2;3]
let my_subset_test11 = not (subset [1;2;3] [1;2])
let my_subset_test12 = not (subset [1;2;3] [])
let my_subset_test13 = not (subset [1;2;3] [4;3;2])
let my_subset_test14 = subset ["a"] ["a";"b";"c"]

let my_equal_sets_test0 = equal_sets [] []
let my_equal_sets_test1 = equal_sets [1] [1]
let my_equal_sets_test2 = equal_sets [1;2] [1;2]
let my_equal_sets_test3 = equal_sets [1;2] [2;1]
let my_equal_sets_test4 = equal_sets [1;2;3] [2;3;1]
let my_equal_sets_test5 = not(equal_sets [] [1])
let my_equal_sets_test6 = not(equal_sets [1] [])
let my_equal_sets_test7 = not(equal_sets [1] [1;2])
let my_equal_sets_test8 = not(equal_sets [1;2] [1])

let my_set_union_test0 = equal_sets (set_union [] []) []
let my_set_union_test1 = equal_sets (set_union [1] [2]) [1;2]
let my_set_union_test2 = equal_sets (set_union [1] [1]) [1]
let my_set_union_test3 = equal_sets (set_union [1;2] [2;3]) [1;2;3]
let my_set_union_test4 = equal_sets (set_union [1;2] []) [1;2]
let my_set_union_test5 = equal_sets (set_union [] [1;2;3]) [1;2;3]

let my_set_intersection_test0 = equal_sets (set_intersection [] []) []
let my_set_intersection_test1 = equal_sets (set_intersection [] [1]) []
let my_set_intersection_test2 = equal_sets (set_intersection [1] []) []
let my_set_intersection_test3 = equal_sets (set_intersection [1;2] [2;3]) [2]
let my_set_intersection_test4 = equal_sets (set_intersection [3;1;3] [1;2;3]) [1;3]
let my_set_intersection_test5 = equal_sets (set_intersection [1;2;3;4] [1;2;3;4]) [1;2;3;4]
let my_set_intersection_test6 = equal_sets (set_intersection [1;2] [3;4]) []

let my_set_diff_test0 = equal_sets (set_diff [] []) []
let my_set_diff_test1 = equal_sets (set_diff [1] []) [1]
let my_set_diff_test2 = equal_sets (set_diff [] [1]) []
let my_set_diff_test3 = equal_sets (set_diff [1] [1]) []
let my_set_diff_test4 = equal_sets (set_diff [1;2] []) [1;2]
let my_set_diff_test5 = equal_sets (set_diff [1;2;3] [1]) [2;3]
let my_set_diff_test6 = equal_sets (set_diff [1;2;3] [3;2;1]) []
let my_set_diff_test7 = equal_sets (set_diff [1;2;3;3;3] [1;2]) [3;3;3]
let my_set_diff_test8 = equal_sets (set_diff [1;2] [1;2;3;4;5]) []

let my_computed_fixed_point_test0 = computed_fixed_point (=) (fun x -> x / 2) 0 = 0
let my_computed_fixed_point_test1 = computed_fixed_point (=) (fun x -> x / 2) 9999 = 0
let my_computed_fixed_point_test2 = computed_fixed_point (=) (fun x -> x *. 2.) 1. = infinity
let my_computed_fixed_point_test3 = computed_fixed_point (=) (fun x -> x * 2) 0 = 0
let my_computed_fixed_point_test4 = computed_fixed_point (=) sqrt 0. = 0.
let my_computed_fixed_point_test5 = computed_fixed_point (=) sqrt 1. = 1.
let my_computed_fixed_point_test8 = computed_fixed_point (fun x y -> abs_float (x -. y) < 1.) (fun x -> x /. 2.) 10. = 1.25
let my_computed_fixed_point_test6 = computed_fixed_point (=) sqrt 10. = 1.
let my_computed_fixed_point_test7 = computed_fixed_point (fun x y -> x > 5) (fun x -> x + 1) 0 = 6

type circular_nonterminals = First | Second
let circular_rules = [First, [N Second]; Second, [N First]]
let my_filter_reachable_test0 = filter_reachable (First, circular_rules) = (First, circular_rules)
let my_filter_reachable_test1 = filter_reachable (Second, List.tl circular_rules) = (Second, List.tl circular_rules)
let my_filter_reachable_test2 = filter_reachable (First, []) = (First, [])

type unconnected_nonterminals = One | Two | Three
let unconnected_rules = [One, [T 1]; Two, [T 2]; Three, [T 3]]
let my_filter_reachable_test3 = filter_reachable(One, unconnected_rules) = (One, [One, [T 1]])
let my_filter_reachable_test4 = filter_reachable(Two, unconnected_rules) = (Two, [Two, [T 2]])
let my_filter_reachable_test5 = filter_reachable(Three, unconnected_rules) = (Three, [Three, [T 3]])
