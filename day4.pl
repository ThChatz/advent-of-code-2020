:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(assoc)).


field(KEY-VAL) -->
    string_without(":", Kcodes),
    atom(':'),
    nonblanks(Vcodes),
    (	blanks_to_nl, !
    ;	whites
    ),
    {
	atom_codes(KEY, Kcodes),
	atom_codes(VAL, Vcodes)
    }.


passport_(KVS) -->
    (	blanks_to_nl,
	{KVS = []}
    ;   field(KV),
	passport_(REST),
	{ KVS = [KV|REST]}
    ).

passport(ASSOC) -->
    passport_(KVS),
    { list_to_assoc(KVS, ASSOC) }.

inputfile(Ps) -->
    (	eos, !, {Ps = []}
    ;	passport(P), !,
	inputfile(Q),
	{ Ps = [P|Q] }
    ).

passport(ASSOC) :-
    REQ_KEYS = [byr, iyr, eyr, hgt, hcl, ecl, pid],
    exclude({ASSOC}/[KEY] >> (get_assoc(KEY, ASSOC, VAL), validate_(KEY, VAL)), REQ_KEYS, []).

validate_(K, V):-
    catch(validate(K, V), _, fail).

validate(byr, VAL) :-
    atom_chars(VAL, Chars),
    number_chars(Num, Chars),
    Num in 1920..2002.

validate(iyr, VAL) :-
    atom_chars(VAL, Chars),
    number_chars(Num, Chars),
    Num in 2010..2020.

validate(eyr, VAL) :-
    atom_chars(VAL, Chars),
    number_chars(Num, Chars),
    Num in 2020..2030.

validate(hgt, VAL) :-
    atom_chars(VAL, Chars),
    append(NumChars, [c, m], Chars),
    number_chars(Num, NumChars),
    Num in 150..193.

validate(hgt, VAL) :-
    atom_chars(VAL, Chars),
    append(NumChars, [i, n], Chars),
    number_chars(Num, NumChars),
    Num in 59..76.

validate(hcl, VAL) :-
    atom_chars(VAL, ['#'|Chars]),
    atom_chars('0123456789abcdef', HEXCHARS),
    length(Chars, 6),
    maplist({HEXCHARS}/[C] >> (member(C, HEXCHARS)), Chars).

validate(ecl, VAL) :-
    member(VAL, [amb, blu, brn, gry, grn, hzl, oth]).

validate(pid, VAL) :-
    atom_chars(VAL, Cs),
    number_chars(_, Cs),
    length(Cs, 9).
    

solve(NUM_ACCEPT) :-
    phrase_from_file(inputfile(Ps), "day4-input.txt"),
    include(passport, Ps, Valid),
    length(Valid, NUM_ACCEPT).
