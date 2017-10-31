
/**** useful.pl *******************************************/

:- use_module(library(charsio)).
:- use_module(library(terms)).

:- unknown(_, fail). /* Needed for QUINTUS/SICSTUS */

:- op(500, yfx, ==>).
:- op(31, yfx, =>).
:- op(500, yfx, <-).
:- op(502, yfx, ^).
:- op(32, yfx, ::).
:- op(501, xfy, &).
:- op(501, xfy, or).

:- op(31, xfy, @).
:- op(700, yfx, --).
:- op(30, yfx, \).
:- op(30, yfx, :).
:- op(32, yfx, <>).
:- op(500, fx, *).
:- op(500, fx, ^).
:- op(500, fx, $$).
:- op(400, yfx, ##).
:- op(400, yfx, #).
:- op(400, yfx, $$).

:- op(500, fx, ?).

with_output_to_atom(G, X) :-
    with_output_to_chars(G, C),
    atom_codes(X, C).

silently(G) :-
    with_output_to_chars(G, _).

default(G) :-
    (G -> true; true).

assert1(P) :-
    retractall(P),
    assert(P).

member(_, V) :-
    var(V),
    !,
    fail.
member(X, [X | _]).
member(X, [_ | T]) :-
    member(X, T).

disjunct(X, _) :-
    var(X),
    !,
    fail.
disjunct((X; Y), D) :-
    !,
    (disjunct(X, D); disjunct(Y, D)).
disjunct(X, X).

addDisjunct((X; Y), X) :-
    var(Y),
    !.
addDisjunct((_; X), D) :-
    addDisjunct(X, D).

nth(0, [X | _], X) :-
    !.
nth(I, [_ | T], X) :-
    J is I-1,
    nth(J, T, X).

contains([H | T], X) :-
    !,
    (contains(H, X);
     contains(T, X)).
contains(X, X) :-
    \+ X = [].
    
delete(_X, E, E) :-
    (var(E); atomic(E)),
    !.
delete(X, [X | T0], T0) :-
    !.
delete(X, [H | T0], [H | T1]) :-
    delete(X, T0, T1).
 
xdelete(X, [X | T0], T0).
xdelete(X, [H | T0], [H | T1]) :-
    xdelete(X, T0, T1).

deleteAll(_X, [], []).
deleteAll(X, [X | T0], T1) :-
    !,
    deleteAll(X, T0, T1).
deleteAll(X, [H | T0], [H | T1]) :-
    deleteAll(X, T0, T1).

append([], L, L).
append([H | T], L, [H | A]) :-
    append(T, L, A).

vappend(V, L, L) :-
    var(V),
    !.
vappend([H | T], L, [H | A]) :-
    vappend(T, L, A).

extend(X1, X2) :-
    X1 == X2,
    !.
extend(V, L) :-
    var(V),
    !,
    V = L.
extend([_X | T], L) :-
    extend(T, L).

extend1(V, X) :-
    var(V),
    !,
    V = [X | _].
extend1([_X | T], X) :-
    extend1(T, X).
    
substitute(X, Y, K, X) :-
    Y == K,
    !.
substitute(_X, _Y, V, V) :-
    var(V),
    !.
substitute(_X, _Y, V, V) :-
    atomic(V),
    !.
substitute(X, Y, [H0 | T0], [H1 | T1]) :-
    !,
    substitute(X, Y, H0, H1),
    substitute(X, Y, T0, T1).
substitute(X, Y, A0, A1) :-
    A0 =.. L0,
    substitute(X, Y, L0, L1),
    A1 =.. L1.

imember(X, [H | _T]) :-
    X == H,
    !.
imember(X, [_H | T]) :-
    nonvar(T),
    imember(X, T).

last([X], X).
last([_ | T], X) :-
    last(T, X).

compareTerms(X, X, _) :-
    !.
compareTerms(X, Y, 0) :-
    (var(X); var(Y); atomic(X); atomic(Y)),
    !,
    format('~w != ~w~n', [X, Y]).
compareTerms([H0 | T0], [H1 | T1], I) :-
    !,
    ((compareTerms(H0, H1, I),
      nonvar(I));
     (compareTerms(T0, T1, I),
      nonvar(I))).
compareTerms(X, Y, J) :-
    X =.. [F | ARGSX],
    Y =.. [F | ARGSY],
    !,
    compareTerms(ARGSX, ARGSY, I),
    ((nonvar(I), I < 2) ->
     (J is I+1,
      tab(J),
      format('~w != ~w~n', [X, Y]));
     true).
compareTerms(X, Y, 0) :-
    format('~w != ~w~n', [X, Y]),
    fail.

compare(X, Y) :-
    compareTerms(X, Y, _N),
    fail.

compareEdges(index@X, index@Y) :-
    call(X),
    call(Y),
    compare(X, Y),
    fail.

qsplit(X, [H | T], L1, R1, TEST) :-
    qsplit(X, T, L0, R0, TEST),
    TASK =.. [TEST, H, X],
    (TASK ->
        (L1 = [H | L0], R1 = R0);
        (L1 = L0, R1 = [H | R0])).
qsplit(_X, [], [], [], _TEST).

% qsort(LIST, SORTED, ACC, TEST) 
qsort([], X, X, _TEST).
qsort([H | T], SORTED, X, TEST) :-
    qsplit(H, T, L, R, TEST),	% L is things in T before H, R is the rest
    qsort(L, SORTED, [H | Y], TEST),
    qsort(R, Y, X, TEST).

qsort(X, S, O) :-
    qsort(X, S, [], O).
qsort(X, S) :-
    qsort(X, S, @<).

mergeSorted(L0, [], L0, _COMP) :-
    !.
mergeSorted([], L1, L1, _COMP) :-
    !.
mergeSorted([H0 | T0], [H1 | T1], [H | T], COMP0) :-
    COMP1 =.. [COMP0, H0, H1],
    (COMP1 ->
     (H = H0,
      mergeSorted(T0, [H1 | T1], T, COMP0));
     (H = H1,
      mergeSorted([H0 | T0], T1, T, COMP0))).

mergeSort(L0, L1, L) :-
    mergeSorted(L0, L1, L, @<).

insert(X, [], [X]) :-
    !.
insert(X, [H | T], [X, H | T]) :-
    X @< H,
    !.
insert(X, [H | T0], [H | T1]) :-
    insert(X, T0, T1).

reverse(L0, L1) :-
    reverse(L0, [], L1).

reverse([], L, L).
reverse([H | T0], L0, L1) :-
    reverse(T0, [H | L0], L1).

% If you've already got a value, increment it; otherwise initialise to 0
% This can be used with any counter: bit like ++counter, but in Prolog

newCounter(K, N) :-
    (retract(counter(K, M)) ->
	N is M+1;
	N = 0),
    assert(counter(K, N)).

set(FLAG, UNSET) :-
    (?FLAG ->
     UNSET = true;
     (set(FLAG), UNSET = unset(FLAG))).

set(FLAG) :-
   (?FLAG ->
       format('~w already set~n', [FLAG]);
       (assert1(FLAG), assert(flag(FLAG)))).

unset(FLAG, RESET) :-
    (?FLAG ->
     (unset(FLAG), RESET=set(FLAG));
     RESET = true).

unset(FLAG) :-
   ((retract(flag(FLAG)), retract(FLAG)) ->
    true;
    format("~w wasn't set", [FLAG])).

:- op(500, fx, ?).

?FLAG :-
    flag(FLAG).

message(F, A) :-
    (?printing ->
	format(F, A);
	true).

trigger(X, G) :-
    nonvar(X),
    X = (A, B),
    !,
    trigger(A, trigger(B, G)).
trigger(X, G) :-
    nonvar(X),
    X = (A; B),
    !,
    trigger(A, G),
    trigger(B, G).
trigger(X, G) :-
    when(nonvar(X), (?inPretty -> true; G)).

rev(L, R) :-
    rev(L, [], R).

rev([], L, L).
rev([H | T], L, R) :-
    rev(T, [H | L], R).

split(L0, SEP, [C | L2]) :-
    (append(C, [SEP | L1], L0);
     (member(X, SEP), append(C, [X | L1], L0))),
    !,
    split(L1, SEP, L2).
split(L, _SEP, [L]).

makeList(A, L) :-
    atomic(A),
    !,
    atom_chars(A, C),
    makeList(C, L).
makeList([H | T], L):-
    number(H),
    !,
    split([H | T], [0' , 0''], L0),
    findall(A, (member(C, L0), atom_chars(A, C)), L).
makeList(L, L).

add1([V | _], X) :-
    var(V),
    !,
    V = X.
add1([_ | T], X) :-
    add1(T, X).

fixlist([]) :-
    !.
fixlist([_ | T]) :-
    fixlist(T).

gensym(reset(A)) :-
    retractall(gensymcounter(A, _)).

gensym(reset) :-
    gensym(reset(_)).

gensym(A, X) :-
    (retract(gensymcounter(A, I)) ->
     J is I+1;
     J = 0),
    assert(gensymcounter(A, J)),
    number_chars(J, JCHARS),
    atom_chars(JA, JCHARS),
    atom_concat(A, JA, X).

list2conjuncts([X], X) :-
    !.
list2conjuncts([H | T], (H, C)) :-
    list2conjuncts(T, C).

list2disjuncts([X], X) :-
    !.
list2disjuncts([H | T], (H; D)) :-
    list2disjuncts(T, D).

msubsume(X, Y) :-
    subsumes_chk(X, Y),
    subsumes_chk(Y, X).

min(I, J, K) :-
    (I < J -> K = I; K =J).
max(I, J, K) :-
    (I > J -> K = I; K =J).

ucase(U) :-
    atom_chars(U, [C | _]),
    C >= 0'A,
    C =< 0'Z.

catch(G, MSG) :-
    catch(G, EXCEPTION, (cpretty(EXCEPTION::MSG), throw(EXCEPTION))).