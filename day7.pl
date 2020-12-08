:-use_module(library(clpfd)).
:-use_module(library(dcg/basics)).
:-use_module(library(yall)).


:- dynamic(contain/2).

:- op(900, yfx, contain).

:- op(600, fx, *).
*_.
*(_,_,_).

%% bright gray bags contain 2 bright gold bags, 5 dull lavender bags.

%% "bright gray bags contain 2 bright gold bags, 5 dull lavender bags."

color(C) -->
    nonblanks(CLR_1),
    blank,
    nonblanks(CLR_2),
    { char_code('_', Und), append(CLR_1, [Und|CLR_2], C_codes), atom_codes(C, C_codes) }.

bag_type(B) -->
    color(C),
    blank,
    ( atom(bags) ; atom(bag) ),
    { atomic_list_concat([C, bags], '_', B)}.

rule(B contain Bs) -->
    bag_type(B),
    blank,
    atom(contain),
    blank,
    bag_contents(Bs).

bag_contents([N*B|NBs]) -->
    integer(N),
    blank,
    bag_type(B),
    bag_contents_(NBs).

bag_contents([]) -->
    atom('no other bags.').

bag_contents_(X) -->
    atom(','),
    blank,
    bag_contents(X).

bag_contents_([]) -->
    atom('.').

rules([R|Rs]) -->
    rule(R),
    blanks,
    rules(Rs).

rules([]) -->
    blanks,
    eos.

read_input :-
    phrase_from_file(rules(Rs), "day7-input.txt"),
    maplist(assertz, Rs).

is_contained_in(Y, X, 1) :-
    X contain Ys,
    member(_*Y, Ys).

is_contained_in(Y, X, DEPTH) :-
    DEPTH #> 1,
    DEPTH_ #= DEPTH - 1,
    is_contained_in(Y, Z, DEPTH_),
    is_contained_in(Z, X, 1).

solve1(Sol, Num) :-
    findall(X, (between(1, 11, I), is_contained_in(shiny_gold_bags, X, I)), Xs),
    list_to_set(Xs, Sol),
    length(Sol, Num).

contains_total(B, T) :-
    B contain Xs,
    maplist([N*X, T] >> (contains_total(X, T_), T #= (T_ + 1) * N), Xs, Ts),
    foldl([X, X0, Y] >> (Y #= X + X0), Ts, 0, T).
    
