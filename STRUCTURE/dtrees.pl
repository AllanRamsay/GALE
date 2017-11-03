/**** DTREES.PL *******************************************************/

dtrees(I, D) :-
    number(I),
    !,
    reconstruct(I, R),
    dtrees(R, D).
dtrees([], []) :-
    !.
dtrees([H0 | T0], [H1 | T1]) :-
    !,
    dtrees(H0, H1),
    dtrees(T0, T1).
dtrees(X, {L, D1}) :-
    getLabel(X, L),
    dtrees(dtrs@X, D0),
    sort(D0, D1).

getLabel(X, L) :-
    simpleTheta(theta@X, THETA1),
    underlyingForm(core@X, TXT),
    with_output_to_atom(format('~w:~w', [THETA1, TXT]), L).

simpleTheta(THETA0, THETA1) :-
    default(THETA0 = arg(*)),
    THETA0 =.. [F, A0],
    default(A0 = ???),
    A0 =.. [A1 | _],
    THETA1 =.. [F, A1].

getDtrees :-
    spanning_edge(X),
    !,
    dtrees(index@X, D),
    pretty(D).