
digit(X:N) :-
    cmember(X:N,
	   [one:1, two:2, three:3, four:4, five:5, six:6, seven:7, eight:8, nine:9]).
digit(N:N) :-
    number(N),
    N < 10.

teen(X:N) :-
    cmember(X:N,
	   [eleven:11, twelve:12, thirteen:13, fourteen:14, fifteen:15, sixteen:16, seventeen:17, eighteen:18, nineteen:19]).
teen(N:N) :-
    number(N),
    N > 10,
    N < 20.

ten(X:N) :-
    cmember(X:N,
	   [twenty:20, thirty:30, forty:40, fifty:50, sixty:60, seventy:70, eighty:80, ninety:90]).
ten(N:N) :-
    number(N),
    N > 9,
    N < 99,
    0 is N rem 10.

number1_100([H0, H1 | T0], T0, N) :-
    ten(H0:N0),
    digit(H1:N1),
    !,
    N is N0+N1.
number1_100([H | T], T, N) :-
    (digit(H:N); teen(H:N); ten(H:N)).

number1_100a([and | T0], T1, N) :-
    number1_100(T0, T1, N).

number1_1000([H0, hundred | L0], Ln, N) :-
    digit(H0:N0),
    !,
    (number1_100a(L0, Ln, N1) ->
	true;
	(Ln = L0, N1 = 0)),
    N is N0*100+N1.
number1_1000(L0, L1, N) :-
    number1_100(L0, L1, N).

number1_1000000(L0, Ln, N) :-
    number1_1000(L0, L1, N0),
    (L1 = [thousand | L2] ->
	(((number1_100a(L2, Ln, N1); number1_1000(L2, Ln, N1)) -> true;
	  (Ln = L2, N1 = 0)),
	    N is N0*1000+N1);
	(Ln = L1, N = N0)).

number(L0, Ln, N) :-
    number1_1000000(L0, L1, N0),
    (L1 = [million | L2] ->
	(((number1_100a(L2,Ln, N1); number1_1000000(L2, Ln, N1)) -> true;
	  (Ln = L2, N1 = 0)),
	    N is N0*1000000+N1);
	(Ln = L1, N = N0)).

removeNumbers([], []).
removeNumbers(L0, [N | Ln]) :-
    number(L0, L1, N),
    !,
    removeNumbers(L1, Ln).
removeNumbers([H | T0], [H | T1]) :-
    removeNumbers(T0, T1).

money([], []).
money(['$', N | T0], [N, '$' | T1]) :-
    !,
    money(T0, T1).
money(['£', N | T0], [N, '£' | T1]) :-
    !,
    money(T0, T1).
money([H | T0], [H | T1]) :-
    money(T0, T1).

substitutions([], []).
substitutions([H0 | T0], [H1 | T1]) :-
	cmember(H0=H1, ['('='ORB', ')'='CRB', '\''='SSS']),
	!,
	substitutions(T0, T1).
substitutions([H | T0], [H | T1]) :-
	substitutions(T0, T1).