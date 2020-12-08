:- use_module(library(clpfd)).
:- use_module(library(yall)).
:- use_module(library(dcg/basics)).

eol --> "\n".
eol --> eos.

group([]) --> eol, !.
group([A|As]) -->
    nonblanks(A_), {string_chars(A_, A)}, eol, group(As).

groups([]) --> blanks, eos, !.
groups([G|Gs]) --> group(G), groups(Gs).

in_all_groups(Val, Gs) :-
    include(member(Val), Gs, Gs_with_Val),
    length(Gs, L),
    length(Gs_with_Val, L).

common(Gs, Cs) :-
    findall(V, in_all_groups(V, Gs), Cs).

unique_yes(Group, Count) :-
    foldl(union, Group, [], Uniques),
    length(Uniques, Count).

all_yes(Group, Count) :-
    nth1(1, Group, First),
    foldl(intersection, Group, First, Uniques),
    length(Uniques, Count).


solve(Sum) :-
    phrase_from_file(groups(Gs), "day6-input.txt"),
    maplist(unique_yes, Gs, Counts),
    foldl([X, X0, Y] >> (Y #= X + X0), Counts, 0, Sum).

solve2(Sum) :-
    phrase_from_file(groups(Gs), "day6-input.txt"),
    maplist(all_yes, Gs, Counts),
    foldl([X, X0, Y] >> (Y #= X + X0), Counts, 0, Sum).
