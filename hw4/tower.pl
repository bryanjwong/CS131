% 1. tower

tower(N, T, C) :-
    check_rowlen(T, N),
    check_collen(T, N),
    check_domain(T, N),
    maplist(fd_all_different, T),
    transpose(T, TT),
    maplist(fd_all_different, TT),
    C = counts(Top, Bot, Left, Right),
    check_edge(TT, Top),
    check_edge_rev(TT, Bot),
    check_edge(T, Left),
    check_edge_rev(T, Right).

check_rowlen(T, N) :-
    length(T, N).

check_collen([], _).
check_collen([HD | TL], N) :-
    length(HD, N),
    check_collen(TL, N).

check_domain([], _).
check_domain([HD | TL], N) :-    
    fd_domain(HD, 1, N),
    check_domain(TL, N).

transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).

check_edge([], []).
check_edge([HD | TL], [VHD | VTL]) :-
    check_list(HD, VHD),
    check_edge(TL, VTL).

check_edge_rev([], []).
check_edge_rev([HD | TL], [VHD | VTL]) :-
    reverse(HD, RHD),
    check_list(RHD, VHD),
    check_edge_rev(TL, VTL).

check_list(L, V) :-
    count_visible(L, Count, 0),
    V #= Count.
    
count_visible([], 0, _).
count_visible([HD | TL], Count, Max) :-
    % HD is visible
    HD #> Max,
    count_visible(TL, NewCount, HD),
    Count is NewCount+1.
count_visible([HD | TL], Count, Max) :-
    % HD is not visible
    HD #< Max,
    count_visible(TL, Count, Max).

% 2. plain_tower

plain_tower(N, T, C) :-
    check_rowlen(T, N),
    C = counts(Top, Bot, Left, Right),
    process_rows(N, T, Left, Right),
    transpose(T, TT),
    process_rows(N, TT, Top, Bot).

process_rows(_, [], [], []).
process_rows(N, [HD | TL], [FHD | FTL], [BHD | BTL]) :-
    length(HD, N),
    maplist(between(1, N), HD),
    check_unique(HD),
    count_visible_plain(HD, 0, 0, FHD),
    reverse(HD, RHD),
    count_visible_plain(RHD, 0, 0, BHD),
    process_rows(N, TL, FTL, BTL).
    
check_unique(L):-
    sort(L, Sorted),
    length(L, Len1),
    length(Sorted, Len2),
    Len1 == Len2.
    
count_visible_plain([], Acc, _, Count) :- Count is Acc.
count_visible_plain([HD | TL], Acc, Max, Count) :-
    % HD is visible
    HD > Max,                                 
    NewAcc is Acc+1,
    count_visible_plain(TL, NewAcc, HD, Count).
count_visible_plain([HD | TL], Acc, Max, Count) :-
    % HD is not visible
    HD < Max,                                                                  
    count_visible_plain(TL, Acc, Max, Count).

% Performance Tests

test_tower(T) :-
    statistics(cpu_time, [Start | _]),
    tower(5, _,
         counts([2,3,2,1,4],
                [3,1,3,3,2],
                [4,1,2,5,2],
                [2,4,2,1,2])),
    statistics(cpu_time, [End | _]),
    T is (End - Start).

test_plain_tower(T) :-
    statistics(cpu_time, [Start | _]),
    plain_tower(5, _,
         counts([2,3,2,1,4],
                [3,1,3,3,2],
                [4,1,2,5,2],
                [2,4,2,1,2])),
    statistics(cpu_time, [End | _]),
    T is (End - Start).

speedup(Ratio) :-
    test_tower(T1),
    test_plain_tower(T2),
    Ratio is T2/T1.	       
	       
% 3. ambiguous
    
ambiguous(N, C, T1, T2) :-
    tower(N, T1, C),
    tower(N, T2, C),
    T1 \= T2.
