lexicon([], []).

%%%%%Exceptional words:

lexicon(['T', 'A', w, s], ['T','A', w, w, s]):-
	!.
lexicon([d, 'A', w, d], [d,'A', w, w, d]):-
	!.
%%%%%%%Numbers
lexicon(['1'], [w,'A', 'H', i, d]):-
	!.
lexicon(['2'], [<,i, v, n, 'A', n, i]):-
	!.
lexicon(['3'], [v, a, l, 'A', v, a ,p]):-
	!.
lexicon(['4'], [>, a, r, b, a, 'E', a, p]):-
	!.
lexicon(['5'], [x, a, m, s, a, p]):-
	!.
lexicon(['6'], [s, i, t, ~, a, p]):-
	!.
lexicon(['7'], [s, a, b, 'E', a, p]):-
	!.
lexicon(['8'], [v, a, m, 'A', n, i, y, a, p]):-
	!.
lexicon(['9'], [t, i, s, 'E', a, p]):-
	!.
lexicon(['10'], ['E', a, $, r, a, p]):-
	!.

%%Abbreviations:

lexicon(['10'], ['E', a, $, r, a, p]):-
	!.

lexicon([H| T0], [H| T1]) :-
	lexicon(T0, T1).