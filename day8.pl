:-use_module(library(clpfd)).
:-use_module(library(yall)).
:-use_module(library(dcg/basics)).
:-use_module(library(assoc)).

:- op(900, fy, nop).
:- op(900, fy, acc).
:- op(900, fy, jmp).

:- op(600, fx, *).
*_.
*(_,_,_).

:- set_prolog_flag(double_quotes, codes).


:-dynamic(line/2).

replace(List, Elem, Replacement, List2) :-
    nth0(N, List, Elem),
    append(A, [Elem|B], List),
    length(A, N),
    append(A, [Replacement|B], List2).


instr(Instr, LineNo) -->
    nop_instr(Instr, LineNo)
    ;    acc_instr(Instr, LineNo)
    ;    jmp_instr(Instr, LineNo).

nop_instr(line(LineNo, nop(P)), LineNo) -->
    atom(nop),
    blank,
    integer(P).

acc_instr(line(LineNo, acc(P)), LineNo) -->
    atom(acc),
    blank,
    integer(P).

jmp_instr(line(LineNo, jmp(P)), LineNo) -->
    atom(jmp),
    blank,
    integer(P).

line(LineNo) -->
    instr(Instr, LineNo),
    blank,
    { assertz(Instr) }.

lines -->
    lines(0).

lines(LineNo) -->
    line(LineNo),
    { LineNo_ #= LineNo + 1 },
    lines(LineNo_).

lines(_) -->
    blanks,
    *eos.

load_program :-
    phrase_from_file(lines, "day8-input.pl").

nop(_, ACC, PC, ACC, PC_) :-
    PC_ #= PC + 1.

acc(D, ACC, PC, ACC_, PC_) :-
    PC_ #= PC + 1,
    ACC_ #= ACC + D.

jmp(D, ACC, PC, ACC, PC_) :-
    PC_ #= PC + D.

run_prog_until_loop(PROG, V, ACC, PC, V_OUT, ACC_OUT, PC_OUT):-
    nth0(PC, PROG, Instr),
    Instr =.. InstrUnfolded,
    append(InstrUnfolded, [ACC, PC, ACC_, PC_], CallableUnfolded),
    Callable =.. CallableUnfolded,
    call(Callable),
    (	not(get_assoc(PC_, V, t)),
	put_assoc(PC_, V, t, V_),
	run_prog_until_loop(PROG, V_, ACC_, PC_, V_OUT, ACC_OUT, PC_OUT)
    ;	get_assoc(PC_, V, t), V_OUT = V, ACC_OUT = ACC, PC_OUT = PC).

run_prog_until_loop(PROG, V, ACC, PC, V, ACC, PC) :-
    \+nth0(PC, PROG, _).

run_prog_until_loop(PROG, ACC_OUT, PC_OUT) :-
    empty_assoc(V),
    run_prog_until_loop(PROG, V, 0, 0, _, ACC_OUT, PC_OUT).


solve1(ACC_OUT) :- 
    findall(I, line(_, I), PROG),
    run_prog_until_loop(PROG, ACC_OUT, _).

solve2(ACC, PC) :-
    findall(I, line(_, I), PROG),
    replace(PROG, jmp(P), nop(P), NEWPROG),
    run_prog_until_loop(NEWPROG, ACC, PC).

solve2(ACC, PC) :-
    findall(I, line(_, I), PROG),
    replace(PROG, nop(P), jmp(P), NEWPROG),
    run_prog_until_loop(NEWPROG, ACC, PC).
    
