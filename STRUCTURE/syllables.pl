%%%% SYLLABLES.PL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

showChar(Y, X) :-
	grapheme:char@Y <-> C,
	underlying:char@Y <-> U,
	(nonvar(C) ->
	 X = C;
	 nonvar(U) ->
	 X = U;
	 X = ?).

showCharSeq(U, S) :-
	findall(X, (cmember(Y, U), showChar(Y, X)), S).

deleteSukuns([], []).
deleteSukuns([S | T0], T1) :-
	sukun:char@S <-> SUKUN,
	nonvar(SUKUN),
	SUKUN = *(_),
	!,
	deleteSukuns(T0, T1).
deleteSukuns([S | T0], [S | T1]) :-
	underlying:char@S <-> U,
	grapheme:char@S <-> C,
	(var(U) ->
	 (+vowel:char@S,
	  -long:char@S);
	 true),
	((var(U), nonvar(C)) -> atom_chars(C, U);
	 (var(C), nonvar(U)) -> atom_chars(C, U);
	 true),
	deleteSukuns(T0, T1).

%% for underlying forms (can't rememebr why but don't want to throw it away just in case)
vowelsAndConsonants([]).
vowelsAndConsonants([X, Y, Z | T]) :-
	+vowel:char@X,
	-long:char@X,
	diphthong:char@X <-> DX,
	+vowel:char@Y,
	+long:char@Y,
	diphthong:char@Y <-> DY,
	-vowel:char@Z,
	%% \+ artic:char@X = artic:char@Y,
	!,
	DX = +,
	DY = +,
	vowelsAndConsonants(T).
vowelsAndConsonants([X, Y | T]) :-
	vowel:char@X <-> XV,
	vowel:char@Y <-> YV,
	diphthong:char@X <-> DX,
	diphthong:char@Y <-> DY,
	long:char@Y <-> LY,
	!,
	((XV = +,
	  (YV = -;
	   (YV = +, LY = +, DX = +, DY = +));
	  (XV = -, (YV = +; YV = -)))),
	default(DX = -),
	vowelsAndConsonants([Y | T]).
vowelsAndConsonants([X]) :-
	diphthong:char@X <-> DX,
	default(DX = -).

markTransitions([C]) :-
	!,
	-cluster:char@C.
markTransitions([X, Y | T]) :-
	-stop:char@Y,
	!,
	-cluster:char@X,
	markTransitions([Y | T]).
markTransitions([X, Y | T]) :-
	!,
	+cluster:char@X,
	markTransitions([Y | T]).

markSyllables([]).
markSyllables([C0, V, C1, C2 | REST]) :-                                  %%OK%%
 	-vowel:char@C0,
 	+vowel:char@V,
 	-vowel:char@C1,
 	-vowel:char@C2,
 	syllable:char@C0 <-> S,
 	syllable:char@V <-> S,
 	syllable:char@C1 <-> S,
 	-open:syll@S,
 	heavy:syll@S <-> final:pos@syllpos:char@C2,
 	C0 <> onset,
 	V <> nucleus,
 	C1 <> coda,
 	!,
 	(+final:pos@word:char@C2 ->
 	 (C2 <> coda, syllable:char@C2 <-> S);
 	 markSyllables([C2 | REST])).



% markSyllables([C0, V0, V1, C1, C2 | REST]) :-
% 	-vowel:char@C0,
% 	+vowel:char@V0,
% 	+vowel:char@V1,
% 	-vowel:char@C1,
% 	-vowel:char@C2,
% 	syllable:char@C0 <-> S,
% 	syllable:char@V0 <-> S,
% 	syllable:char@V1 <-> S,
% 	syllable:char@C1 <-> S,
% 	-open:syll@S,
% 	+heavy:syll@S,
% 	C0 <> onset,
% 	V0 <> nucleus,
% 	V1 <> nucleus,
% 	C1 <> coda,
% 	!,
% 	(REST = [] ->
% 	 (C2 <> coda, syllable:char@C2 <-> S);
% 	 markSyllables([C2 | REST])).

markSyllables([C0, V0, C1 | REST]) :- %%% this is the previous rule after amendment to cover the heavy syllable (CV(+long)C)
	-vowel:char@C0,
	+vowel:char@V0,
	+long:char@V0,
	-vowel:char@C1,
	syllable:char@C0 <-> S,
	syllable:char@V0 <-> S,
	syllable:char@C1 <-> S,
	-open:syll@S,
	+heavy:syll@S,
	C0 <> onset,
	V0 <> nucleus,
	C1 <> coda,
	+final:pos@word:char@C1,
	!.
	
	% (+final:pos@word:char@C1 ->
	%  (C1 <> coda, syllable:char@C1 <-> S);
	% markSyllables([C1|REST])).


markSyllables([C0, V0, C1, V1 | REST]) :-
	-vowel:char@C0,
	+vowel:char@V0,
	-vowel:char@C1,
	+vowel:char@V1,
	syllable:char@C0 <-> S,
	syllable:char@V0 <-> S,
	+open:syll@S,
	-heavy:syll@S,
	C0 <> onset,
	V0 <> nucleus, %%% it was coda, I changed it
	!,
	markSyllables([C1, V1 | REST]).

% markSyllables([C0, V0, V1, C1, V2 | REST]) :- %%%111
% 	-vowel:char@C0,
% 	+vowel:char@V0,
% 	+vowel:char@V1,
% 	-vowel:char@C1,
% 	+vowel:char@V2,
% 	syllable:char@C0 <-> S,
% 	syllable:char@V0 <-> S,
% 	syllable:char@V1 <-> S,
% 	+open:syll@S,
% 	-heavy:syll@S,
% 	C0 <> onset,
% 	V0 <> nucleus,
% 	V1 <> nucleus,
% 	!,
% 	markSyllables([C1, V2 | REST]).

markSyllables([C0, V0, C1]) :-
	-vowel:char@C0,
	+vowel:char@V0,
	-vowel:char@C1,
	syllable:char@C0 <-> S,
	syllable:char@V0 <-> S,
	syllable:char@C1 <-> S,
	-open:syll@S,
	-heavy:syll@S,
	C0 <> onset,
	V0 <> nucleus,
	C1 <> coda.

% markSyllables([C0, V0, V1, C1]) :- %%%222
% 	-vowel:char@C0,
% 	+vowel:char@V0,
% 	-vowel:char@C1,
% 	syllable:char@C0 <-> S,
% 	syllable:char@V0 <-> S,
% 	syllable:char@V1 <-> S,
% 	syllable:char@C1 <-> S,
% 	-open:syll@S,
% 	-heavy:syll@S,
% 	C0 <> onset,
% 	V0 <> nucleus,
% 	V1 <> nucleus,
% 	C1 <> coda.

markSyllables([C0, V0 | REST]) :-
	-vowel:char@C0,
	+vowel:char@V0,
	syllable:char@C0 <-> S,
	syllable:char@V0 <-> S,
	+open:syll@S,
	-heavy:syll@S,
	C0 <> onset,
	V0 <> nucleus,
	markSyllables(REST).

% markSyllables([C0, V0, V1 | REST]) :-   %%% 333
% 	-vowel:char@C0,
% 	+vowel:char@V0,
% 	+vowel:char@V1,
% 	syllable:char@C0 <-> S,
% 	syllable:char@V0 <-> S,
% 	syllable:char@V1 <-> S,
% 	+open:syll@S,
% 	-heavy:syll@S,
% 	C0 <> onset,
% 	V0 <> nucleus,
% 	V1 <> nucleus,
% 	markSyllables(REST).

markSyllables([_X | T]) :-
	!,
	markSyllables(T).
    
findSyllableBoundaries(X, L0, L1) :-
    reverse(underlying@X, L0),
    !, 
    extractUnderlying(L0, _U),
    deleteSukuns(L0, L1),
    doSoftParse(vowelsAndConsonants(L1), vAndC(index@X), X),
    doSoftParse(markTransitions(L1), transitions(index@X), X),
    doSoftParse(markSyllables(L1), syll(index@X), X).

%% Take set of phonemes, get back set of syllables
splitSyllables(U, S) :-
    splitSyllables(U, [], S).

splitSyllables([], X0, [X1]) :-
    !,
    reverse(X0, X1).
splitSyllables([U | UU], X0, S1) :-
    U <> onset,
    reverse(X0, X1),
    splitSyllables(UU, [U], S0),
    !,
    (X1 = [] ->
     S1 = S0;
     S1 = [X1 | S0]).
splitSyllables([U | UU], X, S) :-
    splitSyllables(UU, [U | X], S).

%% L is a word, which is made out of syllables, which are made out of phonemes
%% Each phoneme in a single syllable has the same (unknown) value for stress
%% X is going to be the stressed one

%% work from end looking for a syllable that contains a long vowel

findStressedSyll(L, X) :-              %%%added
    +heavy:syll@syllable:char@S,
    reverse(L, R),
    member(X, R),
    member(S, X),
    !,
    +stress:syll@syllable:char@S.

findStressedSyll(L, X) :-
    +vowel:char@S,
    +long:char@S,
    reverse(L, R),
    member(X, R),
    member(S, X),
    !,
    +stress:syll@syllable:char@S.

%% Reverse the word, check to see that it has at least four syllables
%% X is the 3rd from the end

% findStressedSyll(L, X) :-         %%the original
% 	X <-> [S | _],
% 	reverse(L, [_, _, X, _ | _]),
% 	!,
% 	+stress:syll@syllable:char@S.

findStressedSyll(L, X) :-     %%%edited
	X <-> [S | _],
	reverse(L, [_, _, X | _]),
	!,
	+stress:syll@syllable:char@S.


%% Reverse the word, check to see that it has at least two syllables
%% X is the 2nd from the end
findStressedSyll(L, X) :-
	X <-> [S | _],
	reverse(L, [_, X | _]),
	!,
	+stress:syll@syllable:char@S.

findStressedSyll([X], X) :-
  	X <-> [S | _],
  	+stress:syll@syllable:char@S.

addDefaultStress([], _X).
addDefaultStress([Y | SS], X) :-
    Y <-> [S | _],
    stress:syll@syllable:char@S <-> STRESS,
    (X == Y -> true; STRESS = -),
    addDefaultStress(SS, X).

assignStress(S3) :-
    (markSyllables(S3) -> true; fail),
    splitSyllables(S3, U),
    findStressedSyll(U, X),
    addDefaultStress(U, X).

assignStressToWords([]).
assignStressToWords([W | WORDS]) :-
    assignStress(W),
    assignStressToWords(WORDS).

resplitWords(PHONES, WORDS) :-
    resplitPhones(PHONES, [], WORDS).

resplitPhones([], CURRENT0, WORDS) :-
    (CURRENT0 = [] ->
     WORDS = [];
     (reverse(CURRENT0, CURRENT1),
      WORDS = [CURRENT1])).
resplitPhones([P | PHONES], CURRENT, [WORD | WORDS]) :-
    final:pos@word:char@P == +,
    !,
    reverse([P | CURRENT], WORD),
    resplitPhones(PHONES, [], WORDS).
resplitPhones([P | PHONES], CURRENT, WORDS) :-
    resplitPhones(PHONES, [P | CURRENT], WORDS).
