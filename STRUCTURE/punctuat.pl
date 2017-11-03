 /* 	PUNCTUATION.PL

Mainly about commas, which function as opening and closing brackets.
Fake comma is used to add hidden commas at the start and end of a
sentence, in order that things like "In the park, he saw it." and
"He saw it, in the park." can be treated uniformly.

MUST be loaded before PARSE.PL

 */   

bracket(X, close, TYPE) :-
     X <> saturated,
     type@X <-> TYPE,
     -cat@target@X,
     cat@X <-> bracket(close, TYPE).

bracket(X, open, _TYPE) :-
    +fixed@X,
    cat@X <-> interjection(comma),
    target@X <> x,
    X <> adjunct,
    xend@target@X <-> start@X,
    trigger(index@target@X, npOrS(target@X)),
    -caseDefault@Y,
    -modifiable@X,
    modifier@X <-> *interjection,
    modified@result@X <-> 10,
    Y <> [x, setNoWH],
    +compact@Y,
    args@X <-> [Y, C],
    dir@Y <> xafter,
    -moved:dir@displaced@Y,
    start@Y <-> end@X,
    +compact@Y,
    dir@C <> xafter,
    -moved:dir@displaced@C,
    start@C <-> end@Y,
    bracket(C, close, rb).

npOrS(X) :-
    X <> np.
npOrS(X) :-
    X <> s.

comma1(X) :-
    +fixed@X,
    -modifiable@X,
    cat@X <-> interjection(comma),
    target@X <> x,
    X <> adjunct,
    xend@target@X <-> start@X,
    -modifiable@X,
    trigger(index@target@X,
	    npOrS(target@X)),
    -caseDefault@Y,
    -modifiable@X,
    modifier@X <-> *interjection,
    modified@result@X <-> 10,
    Y <> [x, setNoWH, saturated],
    -value:def@zero@subject@Y,
    +compact@Y,
    args@X <-> [Y, C],
    dir@Y <> xafter,
    -moved:dir@displaced@Y,
    start@Y <-> end@X,
    +compact@Y,
    dir@C <> xafter,
    -moved:dir@displaced@C,
    start@C <-> end@Y,
    comma3(C),
    ptag:tag@tag@X <> tag(comma(1)).

/**
For cases where the INITIAL comma is missing, so the closing one has to
act as the opener. In this case the argument is to the left and the target
is to the right and is a sentence.
*/

comma1(X) :-
    +fixed@X,
    cat@X <-> interjection(comma),
    target@X <> s,
    X <> adjunct,
    xstart@target@X <-> end@X,
    +fixed@target@X,
    -caseDefault@Y,
    -modifiable@X,
    modified@result@X <-> 10,
    Y <> [x, setNoWH, saturated],
    dir@Y <> xbefore,
    +compact@Y,
    args@X <-> [Y],
    -value:def@zero@subject@Y,
    target@Y <> s,
    +compact@Y,
    start@Y <-> 0,
    xend@Y <-> start@X,
    ptag:tag@tag@X <> tag(comma(1)).

/**
Comma as list-maker in complex conjunction
*/
comma2(X) :-
     +fixed@X,
    -modifiable@X,
     coordinate_set(X, C1, C2),
     -moved:dir@displaced@C1,
     -moved:dir@displaced@C2,
     theta@C1 <-> arg(identity),
     theta@C2 <-> arg(identity),
     % so we only get A & (B & C)
     -conj@C2,
     dir@C2 <> xbefore,
     dir@C1 <> xafter,
     % inherit conjunction from right argument
     conj@X <-> conj@C1,
     conj@C1 <-> infix(_TYPE),
     C1 <> genuine,
     C2 <> genuine,
     arg@X <-> arg@C1,
     definite@X <-> identity,
     trigger(index@C1, nonvar(cat@C1)),
     args@X <-> [C1, C2 | args@C2],
     % inherit semantics from right argument
     semantics@X <-> semantics@core@C1,
    ptag:tag@tag@X <> tag(comma(2)). 

comma3(C) :-
    cat@C <-> interjection(comma),
    -conj@C,
    C <> [word, saturated].

/**
Invisible closing comma/dash under the full-stop
*/
zeroComma(N) :-
    current_language(language@C),
    cat@C <-> interjection(_),
    C <> [setNoWH, saturated],
    start@C <-> N,
    end@C <-> N,
    xstart@C <-> N,
    xend@C <-> N,
    span@C <-> 0,
    -realised@C,
    newindex(index@C),
    -cat@target@C,
    text@C <-> '0comma',
    text@core@C <-> '0comma',
    dtrs@C <-> [],
    penalise(C, 12),
    post_defaults(C),
    assertWhen(complete(C)).
comma(C) :-
    comma1(C).
comma(C) :-
    comma2(C).

isComma(X) :-
    X <> word, text@core@X <-> ','.

/*
'repaired' was introduced solely to control interactions between the
various kinds of comma-generated phrases. It is defined in types.pl,
and mentioned hardly anywhere else. Do

	% grep repaired *.pl

to find out where it is used.  An experiment: do some examples with
the lexical entries as they are, then comment out the lines that say
that these things are repaired and see what happens.  Another
experiment: remove the fact that commas could be repairs or
hesitations from the definition of comma, and introduce instead a new
lexical entry of type repair. See what happens. This one should behave
much better.  Don't forget to assert soft(_) when you're doingexperiments with repairs and hesitations (I'll shoot you if you come
and that was the problem)
*/

repair(X) :-
     +silence@X,
     +repaired@X,
     X <> inflected,
     modified@X <-> -2,
     args@X <-> [LEFT, RIGHT | args@RIGHT],
     -moved:dir@displaced@LEFT,
     -moved:dir@displaced@RIGHT,
     dir@LEFT <> xbefore,
     dir@RIGHT <> xafter,
     -ellipse@LEFT,
     LEFT <> [genuine, no_case_default],
     RIGHT <> [genuine, no_case_default],
     +fixed@X,
     % This says what the left and right hand sides of a repair have in common
     % try being more specific, e.g. by demanding that they share head features,
     % (head, agree, vform, specf)
     % try being less specific, e.g. by allowing them not to share arg lists     
     [cat, args]@RIGHT <-> [cat, args]@LEFT,
     % theta@LEFT <-> arg(xxx),
     -conj@RIGHT,
     theta@RIGHT <-> arg(identity),
     nonfoot@X <-> nonfoot@RIGHT,
     nonfoot@RIGHT <-> nonfoot@X.
  
hesitation(X) :-
     +silence@X,
     +repaired@X,
     X <> inflected,
     modifiable@X <-> modifiable@RIGHT,
     +fixed@X,
     -moved:dir@displaced@RIGHT,
     args@X <-> [RIGHT | args@RIGHT],
     dir@RIGHT <> xafter,
     -conj@RIGHT,
     RIGHT <> [genuine, no_case_default],
     +compact@RIGHT,
     nonfoot@RIGHT <-> nonfoot@X,
     positions@RIGHT <-> positions@core@RIGHT.    

%% hyphen in compound noun
hyphen(X) :-
     +fixed@X,
     X <> noun,
     [head, mod]@X <-> [head, mod]@Y,
      +fixed@target@Y,
     %% syntax@target@M <-> syntax@Y,
     text@Y <-> text@core@Y,
     text@M <-> text@core@M,
     args@X <-> [Y, M],
     M <> noun,
     end@Y <-> start@X,
     start@M <-> end@X,
     dtrs@Y <-> [],
     dtrs@M <-> [],
     dir@M <> xafter,
     dir@Y <> xbefore,
     semantics@X <-> identity,
     modifier@X <-> *noun_as_mod,
     ptag:tag@tag@X <> tag(dash(0)).

basicDash(X) :-
    +fixed@X,
    cat@X <-> interjection(dash),
    X <> adjunct,
    target@X <> [x, testNoWH],
    xend@target@X <-> start@X,
    -caseDefault@Y,
    -modifiable@X,
    modified@result@X <-> 10,
    Y <> [x, testNoWH, saturated],
    args@X <-> [Y, C],
    dir@C <> xafter,
    cat@C <-> interjection(dash),
    C <> word,
    dir@Y <> xafter,
    +compact@Y,
    start@Y <-> end@X.

whichDash(REAL, TARGET) :-
    (REAL = - ->
	s(TARGET);
	trigger(index@TARGET, npOrS(TARGET))).

mdash(X) :-
    basicDash(X),
    args@X <-> [_Y, C],
    ptag:tag@tag@X <> tag(dash(1)),
    trigger(realised@C, whichDash(realised@C, target@X)).

ndash(X) :-
    X <> det0,
    dtarget(X, TARGET),
    dresult(X, RESULT),
    +fixed@X,
    args@X <-> [N1, N2],
    dir@N1 <> xbefore,
    N1 <> [det, word],
    -moved:dir@displaced@N1,
    end@N1 <-> start@X,
    dir@N2 <> xafter,
    N2 <> [det, word],
    -moved:dir@displaced@N2,
    start@N2 <-> end@X,
    TARGET <> noun,
    dir@TARGET <> xbefore,
    -moved:dir@displaced@TARGET,
    start@TARGET <-> end@N2,
    RESULT <> np.
    
    