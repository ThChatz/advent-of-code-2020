:-use_module(library(clpfd)).
:-use_module(library(yall)).

:-include("day3-input.pl").

steps(slope(DY, DX), STEPS) :-
    TARGET #= 322,
    STEPNO #= (TARGET + DY - TARGET mod DY) div DY,
    length(STEPS, STEPNO),
    maplist([XY, X, Y] >> (XY = [Y, X]), STEPS, Xs, Ys),
    foldl({DY}/[Y, Y0, S] >> (Y #= Y0, S #= DY + Y0), Ys, 0, _),
    foldl({DX}/[X, X0, S] >> (X #= X0, S #= DX + X0), Xs, 0, _).

slope_trees(SLOPE, TREES_NO) :-
    slope_cells(SLOPE, Cs),
    include(tree, Cs, Ts),
    length(Ts, TREES_NO).

slope_cells(SLOPE, CELLS) :-
    steps(SLOPE, STEPS),
    maplist([XY, Y, X] >> (XY = [Y, X]), STEPS, Ys, Xs),
    maplist(i_j_cell, Ys, Xs, CELLS).

solve_p2(SLOPES, Ts, PROD) :-
    maplist(slope_trees, SLOPES, Ts),
    foldl([X, X0, Y] >> (Y #= X * X0), Ts, 1, PROD).
