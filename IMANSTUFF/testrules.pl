%%%% TESTRULES.PL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% testrules :-
%   	retractall(matched(_, _, _)),
%   	treebanksent(S),
%   	atom_chars(A, S),
%   	format('testRule("~s", _, _, SAMPA).~n', [S]),
%   	(testRule(S, _CHARS0, _CHARS1, SAMPA) ->
%   	 true;
%   	 (format('testRule failed on ~w~n', [A]),
%   	  fail)),
%   	format('text: ~w~nsampa: ~w~n~n', [A, SAMPA]),
%   	fail.
% testrules :-
%   	setof(H0=H1:A, matched(H0, H1, A), L),
%   	pretty(L),
%   	fail.
% testrules.

transcribe(S0, USERULES0) :-
    atom_chars(S0, S1),
    (number(USERULES0) ->
     USERULES1 = USERULES0;
     (atom_chars(USERULES0, USERULESCHARS),
      number_chars(USERULES1, USERULESCHARS))),
    (testRule(S1, _CHARS0, _CHARS1, SAMPA, USERULES1) ->
     true;
     (format('testRule failed on ~w~n', [S0]),
      fail)),
    format('text: ~w~nsampa: ~w~n',[S0, SAMPA]).

testrules(USERULES1, ID) :-
    sentence(ID, S),
    atom_chars(IDC, ID),
    pretty(IDC),
    atom_chars(A, S),
    (testRule(S, _CHARS0, _CHARS1, SAMPA, USERULES1) ->
     true;
     (format('testRule failed on ~w~n', [A]),
      fail)),
    format('text: ~w~nsampa: ~w~n',[A, SAMPA]),
    fail.
    
testrules(USERULES0) :-
    retractall(matched(_, _, _)),
    (number(USERULES0) ->
     USERULES1 = USERULES0;
     (atom_chars(USERULES0, USERULESCHARS),
      number_chars(USERULES1, USERULESCHARS))),
    testrules(USERULES1, _ID).
testrules(_USERULES) :-
   	setof(H1:A, matched(H1, A), L),
   	pretty(L),
   	fail.
testrules(_USERULES).

test([DIR, PROMPTS0, OUT0, USERULES]) :-
    with_output_to_atom(format('~w/~w', [DIR, PROMPTS0]), PROMPTS1),
    with_output_to_atom(format('~w/~w', [DIR, OUT0]), OUT1),
    catch((compile([PROMPTS1]), tell(OUT1), testrules(USERULES), told),
	  ERRMSG,
	  format('~w~n', [ERRMSG])),
    halt.

test :-
    prolog_flag(argv, ARGS),
    test(ARGS),
    halt.

saveTest :-
    abolish(sentence/2),
    save_program('test.sav', test),
    save_program('transcribe.sav').

matchWords([], [], _A).
matchWords([H0 | T0], [H1 | T1], A) :-
	(matched(H0, H1, _) ->
	 true;
	 assert(matched(H0, H1, A))),
	matchWords(T0, T1, A).

getWordsFromString(CHARS, WORDS) :-
	getWordsFromString(CHARS, [], WORDS).

getWordsFromString([], [], []) :-
	!.
getWordsFromString([], CURRENT, [A]) :-
	!,
	atom_chars(A, CURRENT).
getWordsFromString([32 | CHARS], [], WORDS) :-
	!,
	getWordsFromString(CHARS, [], WORDS).
getWordsFromString([32 | CHARS], CURRENT, [A | WORDS]) :-
	!,
	atom_chars(A, CURRENT),
	getWordsFromString(CHARS, [], WORDS).
getWordsFromString([C | STRING], CURRENT0, WORDS) :-
	append(CURRENT0, [C], CURRENT1),
	getWordsFromString(STRING, CURRENT1, WORDS).

getWordsFromChars(CHARS, WORDS) :-
	getWordsFromChars(CHARS, [], WORDS).

getWordsFromChars([], [], []) :-
	!.
getWordsFromChars([], CURRENT, [CURRENT]) :-
	!.
getWordsFromChars([X | CHARS], [], WORDS) :-
	+initial:pos@word:char@X,
	!,
	getWordsFromChars(CHARS, [sampa:char@X], WORDS).
getWordsFromChars([X | CHARS], CURRENT, [CURRENT | WORDS]) :-
	+initial:pos@word:char@X,
	!,
	getWordsFromChars(CHARS, [sampa:char@X], WORDS).
getWordsFromChars([C | STRING], CURRENT0, WORDS) :-
	append(CURRENT0, [sampa:char@C], CURRENT1),
	getWordsFromChars(STRING, CURRENT1, WORDS).




	