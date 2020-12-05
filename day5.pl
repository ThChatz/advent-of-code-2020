:- use_module(library(clpfd)).
:- use_module(library(yall)).

:- include("day5-input.pl").

:- op(900, fx, *).
*_.

part_vert_(['F'|Vs], RowNum, Row) :-
    RowNum_ #= RowNum div 2,
    part_vert_(Vs, RowNum_, Row).

part_vert_(['B'|Vs], RowNum, Row) :-
    RowNum_ #= RowNum div 2,
    part_vert_(Vs, RowNum_, Row_),
    Row #= Row_ + RowNum_.

part_vert_([], _, 0).

part_vert(Vs, Row) :-
    part_vert_(Vs, 128, Row).


vert_to_hor(['F'|Vs], ['L'|Hs]) :-
    vert_to_hor(Vs, Hs).

vert_to_hor(['B'|Vs], ['R'|Hs]) :-
    vert_to_hor(Vs, Hs).

vert_to_hor([], []).

part_hor(Hs, Col) :-
    vert_to_hor(Vs, Hs),
    part_vert_(Vs, 8, Col).
    

code_id(CODE, ID) :-
    atom_chars(CODE, Chars),
    length(Hs, 3),
    append(Vs, Hs, Chars),
    part_vert(Vs, Row),
    part_hor(Hs, Col),
    ID #= 8 * Row + Col.

pos_id(pos(Row, Col), ID) :-
    Col in 0..7,
    Row in 0..127,
    ID #= 8 * Row + Col.

solve1(ID):-
    limit(1, order_by([desc(ID)], (code(C), code_id(C, ID)))).

my_seat_(ID, IDs) :-
    ID in 0..976,
    pos_id(pos(Row, Col), ID),
    Row #> 0,
    Row #< 127,
    IDn #= ID + 1,
    IDp #= ID - 1,
    member(IDn, IDs),
    \+member(ID, IDs),
    member(IDp, IDs).

solve2(MINE) :-
    findall(ID, order_by([asc(ID)], (code(C), code_id(C, ID))), IDs),
    my_seat_(MINE, IDs).
