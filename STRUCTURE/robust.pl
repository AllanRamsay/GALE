/*

    ROBUST.PL
    
*/

fragment(S) :-
    S <> [s, tensed_form],
    +compact@S,
    +specified@S.
fragment(S) :-
    theta@S <-> lambda(P, P:lambda(Q, someClaimAboutIt(Q) & 'POSITION'(Q, 0))),
    S <> [np].

findFragment(S) :-
    retractall(basic_interpretation(_, _, _, _)),
    retractall(interpretation(_, _, _, _)),
    +realised@S,
    fragment(S),
    retrieve(_I, S),
    % end@S-start@S > 2,
    span@S <-> SPANS,
    span@X <-> SPANX,
    \+ (fragment(X),
	   retrieve(_J, X),
	   SPANX > SPANS,
	   SPANS is SPANS /\SPANX),
    showTree(_, _, S). % showMeaning(_, _, S, true).

robustParse(S) :-
    retractall(bound(_)),
    clean_up,
    assert(bound(300)),
    call_residue((assert1(firstOneOnly),
                  (catch(javaMeanings(S, _LF, _I), ANYERROR, true)
		      -> (nonvar(ANYERROR) ->
			     format('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ~w~n', [ANYERROR]);
			     true);
		         true),
                  findFragment(_)),
                  _),
    !,
    retractall(bound(_)).

startEssay :-
    ((discourse_state(A, B, C, D), D > 0, retract(discourse_state(A, B, C, D)), fail);
	true),
    retractall(child(_, _)),
    retractall(sister(_, _)),
    nextSentence,
    xabolish(utterance/2),
    xabolish(instantiated/1),
    assert(user:cdiscourse(1)).
unknownWords(S, U) :-
    retractall(unknownWord(_)),
    assert1(justWords),
    catch(javaMeanings(S, _LF, _I, english), _ERROR, true),
    (setof(W, unknownWord(W), U) -> true; U = []),
    retractall(justWords),
    U = [_ | _].

unknownWords(S) :-
    unknownWords(S, U),
    pretty(U).

parseAllEssays :-
    retractall(xxxx(_, _, _, _)),
    essay(E, SENTENCES),
    parseAllEssays(E, 1, SENTENCES).

parseAllEssays(E, I, [S | SENTENCES]) :-
    robustParse(S),
    format('ESSAY ~w~n', [S]),
    interpretation(_, X, _, _),
    assertWhen(xxxx(E, I, S, X)),    J is I+1,
    parseAllEssays(E, J, SENTENCES).
