:- include("day9-input.pl").

constraint(PRE, I, N) :-
    I #> PRE,
    I_pre #= I - PRE,
    [J, K] ins I_pre..I,
    J #>= K,
    input(J, M),
    input(K, O),
    N #= M + O.

constraint(PRE, I, N) :-
    I #=< PRE.

solve(PRE, SOLUTION) :-
    findall(constraint(PRE, I, N), input(I, N), Inputs),
    exclude(call, Inputs, SOLUTION).

    
sublist(List, Start, End, Sub) :-
    length(List, Total),
    Start #>= 0,
    Start #=< End,
    Start #= 0 #==> End #>= Start,
    Start #> 0 #==> End #> Start,
    End #=< Total,
    label([Start, End]),
    length(A, Start),
    length(B, End),
    append(B, _Postfix, List),
    append(A, Sub, B).
    

solve2(PRE, MIN-MAX-SUM, D) :-
    solve(PRE, [constraint(_, _, SOL1)]),
    findall(N, order_by([asc(I)], input(I, N)), Is),
    length([_,_|_], D),
    B #= A + D,
    sublist(Is, A, B, IU),
    sum(IU, #=, SOL1),
    min_member(MIN, IU),
    max_member(MAX, IU),
    SUM #= MAX + MIN.
