:-use_module(library(clpfd)).

:-include("day2-input.pl").

policy_password(POL, PASS) :- policy_password2(POL, PASS).

policy_password1(policy(MIN-MAX, C), PASSWD):-
    L #>= MIN,
    L #=< MAX,
    atom_chars(PASSWD, PASSWD_Cs),
    include(=(C), PASSWD_Cs, Cs),
    length(Cs, L).

policy_password2(policy(P1-P2, C), PASSWD) :-
    atom_chars(PASSWD, Cs),
    nth1(P1, Cs, E1),
    nth1(P2, Cs, E2),
    E1 \= E2,
    (	E1 = C ; E2 = C).
    
solve(NUM) :-
    findall(X, (input(X), call(X)), Xs),
    length(Xs, NUM).


    
