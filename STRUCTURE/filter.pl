
%%%% FILTER.PL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/***************************************************************************
Very important, very very complicated. This and
STRUCTURE/position.pl are pretty much the syntactic theory.

\medpara
We're combining two items, usually called \texttt{X} and \texttt{Y},
to make a new item \texttt{Z}. \texttt{X} is the head of the phrases,
\texttt{Y} is either an argument or a modifier. \texttt{Y} is
basically the right kind of thing for its role, or we wouldn't have
got to here, but there are a lot of fine details that can't be checked
until you have a really good look at what's going on. So we have a
\scare{filter}, which I think is is a fairly Chomskyish notion, which
looks at various things. It has three major elements: deal with
WH-marking (and other foot features, only there aren't any); check
extraposition---this is the complicated bit; do other minor checks.

\paragraph{WH-marking:} not too complicated. The WH-markers on
\texttt{X} and \texttt{Y} are merged to get the marker for
\texttt{Z}---I tried doing it using difference lists, but it was
painfully complicated, and appending a couple of quite short lists
doesn't exactly cost a lot. wh_filter/3 enforces the constraint that
there should be no WH-marked dtr following a non-WH-marked dtr, which
is right for English and which I am using for other languages (apart
from Somali, which apparently doesn't care).

\paragraph{Extraposition:} this is a bugger. For English there are
lots of very delicate rules, covering very general issues (WH-marked
items can be left-shifted!) and very specific ones (\eg all the
intricacies of \q{more than six peaches} vs \q{more peaches than
pears}). I am sure that the real facts for other languages are just as
complicated, and that the reason why we haven't got lots of rules for
other languages is that they haven't been looked at
properly. filter_disp/3 leads off to language specific rules
(filterArabicDisplacement/3, \ldots) which then split into \eg
filterLeftArabicDisplacement/3 and filterRightArabicDisplacement/3,
which are then supposed to do something interesting (but as noted only
the English ones are at all developed).

\paragraph{Others:} just a rag-tag of things. Can't get the mood of a
sentence till you know the relative positions of the various elements,
so we have filter_mood/1, we sometimes need to know which constituents
have been shifted around (topicalisation/3) and then there's just one
called whatDoYouThinkOfIt/3 for random constraints and observations.

******************************************************************/

/***
Add a penalty to the score for X: uses a mutable so we can go on
adding more costs
***/

penalise(X, N) :-
    incrementScore(score@X, N).

penalise(X, TEST, N) :-
    (TEST -> penalise(X, N); true).

incrementScore(SCORE, N) :-
    (var(SCORE) ->
	(create_mutable(N, SCORE));
	(get_mutable(S0, SCORE),
	    S1 is S0+N,
	    update_mutable(S1, SCORE))).

/***
	FILTER tidies up once you've made something of the specified type.
*/

filter(_, _, _) :-
    flag(nofilter),
    !.
filter(X, Y, Z) :-
    %% MUST BE DONE FIRST
    %% startTimer(filter),
    wh@X <-> WHX,
    wh@Y <-> WHY,
    wh@Z <-> WHZ,
    !,
    startTimer(filter_wh),
    (WHX = [] ->
	WHZ = WHY;
	WHY = [] ->
	WHZ = WHX;
	append(WHX, WHY, WHZ)),
    %% filter_wh and filter_disp both make use of dynamic dtrs (when
    %% we're using them). For this to do us any good, we have to do them
    %% inside a double negation (we have to do that with filter_disp anyway)
    %% and it turns out to be worth doing them both inside the same one
    %% because then filter_disp can make use of any instantiations that
    %% filter_wh did
    \+ \+ (%% Check that WH-marked items are in acceptable positions
	      filter_wh(X, Y, Z),  
	      stopTimer(filter_wh),
	      startTimer(filter_disp),
	      %% Check that shifted items are in acceptable positions
	      filter_disp(X, Y, Z),
	      true),
    stopTimer(filter_disp),
    ((vlength(WHZ, WH2L), WH2L > 1) -> penalise(Z, 100); true),
    startTimer(topic),
    topicalisation(X, Y, Z),	% Has it undergone topicalisation
    stopTimer(topic),
    startTimer(mood),
    filter_mood(Z),		% Declarative/interrogative/imperative
    stopTimer(mood),
    startTimer(whatDoYouThinkOfIt),
    whatDoYouThinkOfIt(X, Y, Z),
    stopTimer(whatDoYouThinkOfIt),
    % stopTimer(filter),
    goodOrBad(Z),
    true.

goodOrBad(Z) :-
    (\+ \+ (call_residue(getBaseARFF(Z, ARFF), _),
	       tryj48(ARFF, good)) ->
	(useJ48(N) -> penalise(Z, N); true);
	true).

whatDoYouThinkOfIt(_X, _Y, Z) :-
    language@Z <> persian,
    Z <> s,
    -compact@Z,
    testNoWH(Z),
    !,
    fail.

whatDoYouThinkOfIt(X, Y, Z) :-
    language@Z <> arabic,
    Z <> [verb],
    +moved:dir@displaced@Y,
    !,
    (Z <> testNoWH ->
	penalise(Z, 4);
	true),
    (Z <> [saturated, testNoWH] ->
	+compact@Z;
	true),
    (+compact@Z -> true; penalise(Z, 300)),
    ((+clitic@Y) ->
	(Y = subject@Z ->
	    true;
	    pretty(X+Y));
	true).

whatDoYouThinkOfIt(X, Y, Z) :-
    language@Z <> somali,
    Z <> [verb],
    +clitic@Y,
    MODY <-> modified@Y,
    MODA <-> modified@A,
    nonvar(MODY),
    !,
    \+ (element(A, dtrs@X), nonvar(MODA), MODA < MODY).

whatDoYouThinkOfIt(X, Y, Z) :-
    language@X <> english,
    displaced@Y <-> D,
    +moved:dir@D,
    %% changed from xstart to start: Allan, 1/08/07. Fixed "if sleeping
    %% is good for you you should sleep". What has it broken?
    start@core@X <-> XSTART,
    start@core@Y <-> YSTART,
    testNoWH(Y),
    !,
    (XSTART > YSTART ->
	XD is 10*(XSTART-YSTART);
	XD is 23*(YSTART-XSTART)),
    penalise(Z, XD).

whatDoYouThinkOfIt(X, Y, Z) :-
    language@X <> german,
    X <> verb,
    Y <-> subject@X,
    !,
    (start@Y > start@core@X -> penalise(Z, 10); true).

whatDoYouThinkOfIt(_X, _Y, _Z).

filter_mood(Z) :-
    Z <> [clause],
    language@Z <> english,
    -bracketed@Z, 
    -comp@Z,
    index@subject@Z <-> SUBJINDEX,
    tensed@Z <-> TNSD,
    start@core@Z <-> start@core@DUMMYZ,
    [wh, positions, zero]@subject@Z <-> [wh, positions, zero]@subject@DUMMYZ,
    [positions, wh, tensed, mood]@Z
         <-> [positions, wh, tensed, mood]@DUMMYZ,
    nonvar(SUBJINDEX),
    !,
    trigger(TNSD, reallyFilterMood(DUMMYZ)).

filter_mood(Z) :-
    Z <> clause,
    language@Z <> arabic,
    !,
    (testNoWH(Z) ->
	default(Z <> declarative1);
	default(Z <> wh_interrogative1)).

filter_mood(_).
   
reallyFilterMood(Z) :-
    %% var(declarative@Z),
    +tensed@Z,
    start@core@Z <-> J,
    subject@Z <-> S,
    start@S <-> I,
    end@S <-> E,
    value:def@zero@S <-> ZSUBJ,
    interrogative@Z <-> INT,
    !,
    ((var(I); var(J)) ->
	true;
      I < J ->
        %% subject precedes verb -> declarative
    	(S <> testNoWH ->
        	(ZSUBJ = -, Z <> declarative1); % subject is -wh
        	(-imperative@Z,
		    trigger(INT, ((INT = +) -> wh_interrogative1(Z); true))));
     (I = E, J = 0) ->  	% Zero subject -> imperative
	  Z <> imperative1;
      Z <> testNoWH ->
	  Z <> interrogative1;	% Otherwise interrogative
	  Z <> wh_interrogative1).
reallyFilterMood(_).

/***
this to consider the shifted arguments, for example : SVO ,SOV and consider them 
as -MAIN also. Exclude subjectless from this process.
*/

topicalisation(X, Y, Z) :-
    language@Z <> english,
    +moved:dir@displaced@Y,
    before:dir@displaced@Y == +,
    testNoWH(Y), 
    [head, theta, start]@Y <-> [head, theta, start]@T,
    topicalised@X <-> TX,
    topicalised@Z <-> TZ,
    theta@K <-> arg(_),
    theta@L <-> modifier(_),
    subject@X <-> SUBJ,
    theta@Y <-> YTHETA,
    \+ Y = SUBJ,
    !,
    ((YTHETA <-> arg(_), \+ Y = SUBJ) -> \+ cmember(K, TX); true),
    %% Under some careless circumstances modifiers can get mistaken for
    %% subjects, in which case we get horrid circular structures.
    %% Checking that it is indeed an argument stops that
    (YTHETA = arg(_) ->
         (testNoWH(X));
         true),
    append(TX, [T], TZ),
    !,
    % Don't allow arguments to be left-shifted further than modifiers:
    % Allan, 14/09/06
    penalise(Z, 71),
    \+ (cmember(K, TZ), \+ K = SUBJ, cmember(L, TZ), start@K < start@L, testNoWH(K)).
topicalisation(X, Y, Z) :-
    language@Z <> german,
    +moved:dir@displaced@Y,
    before:dir@displaced@Y == +,
    Y <> testNoWH, 
    syntax@Y <-> syntax@T,
    topicalised@X <-> TX,
    topicalised@Z <-> TZ,
    !,
    append([T, prev(_)], TX, TZ).
topicalisation(X, _Y, Z) :-
    (topicalised@X=topicalised@Z -> true; true).

% WH items must precede non-WH items

% Y is not WH-marked, X is: so Y must not precede X

filter_wh(X, _Y, _Z) :-
    language@X <> somali,
    !.

filter_wh(X, Y, _Z) :-
    X <> verb,
    \+ X <> saturated,
    testNoWH(Y),
    +realised@Y,
    xstart@X <-> START_X,
    xstart@Y <-> START_Y,
    X <> checkWHMarked(_XXX),
    !,
    ((var(START_X); var(START_Y)) ->
	fail;
	START_Y > START_X).

% Y is WH-marked: there must not be an earlier -WH dtr

filter_wh(X, Y, _Z) :-
    X <> verb,
    D <> setNoWH,
    Y <> checkWHMarked(YYY),
    xstart@Y <-> START_Y,
     % for imperatives - HG 02/08/01
    nonvar(START_Y),
    xstart@D <-> START_D,
    dtrs@X <-> X_DTRS,
    !,
    START_Y < xstart@X,  % Added for Arabic: OK for others????
    \+ (element(D, X_DTRS), START_D <  START_Y),
    (cat@YYY == thatphrase ->
        testNoWH(X);
        true).    

filter_wh(_, _, _).

noCrossings(X, Y) :-
    xstart@X <-> STARTX,
    xend@X <-> ENDX,
    xstart@Y <-> STARTY,
    xend@Y <-> ENDY,
    ((STARTY > STARTX, STARTY < ENDX) ->
        ENDY < ENDX;
     (ENDY > STARTX, ENDY < ENDX) ->
         STARTY < STARTX;
        true).
    
filter_disp(X, Y, _) :-
    -moved:dir@displaced@Y,
    !,
    noCrossings(X, Y).  
    
filter_disp(X, Y, Z) :-
    language@X <> english,
    !,
    % noCrossings(X, Y),
    filterEnglishDisplacement(X, Y, Z).    
    
filter_disp(X, Y, Z) :-
    language@X <> german,
    !,
    filterGermanDisplacement(X, Y, Z).
    
filter_disp(X, Y, Z) :-
    language@X <> persian,
    !,
    filterPersianDisplacement(X, Y, Z).
  
filter_disp(X, Y, Z) :-
    language@X <> arabic,
    !,
    filterArabicDisplacement(X, Y, Z).  
  
filter_disp(X, Y, Z) :-
    language@X <> somali,
    !,
    filterSomaliDisplacement(X, Y, Z).  

between(X, START, END) :-
	retrieve(_I, X),
	nonvar(start@X),
	nonvar(end@X),
	start@X > START,
	end@X < END.

filterEnglishDisplacement(X, Y, _Z) :-
	max(end@X, end@Y, END),
	min(start@X, start@Y, START),
	OB <> bracket(open, T),
	CB <> bracket(close, T),
	(between(OB, START, END) ->
	    \+ between(CB, START, END);
	    between(CB, START, END)),
	!,
	fail.
	
filterEnglishDisplacement(X, Y, Z) :-
    +after:dir@displaced@Y,
    +compact@Y,
    filterEnglishRightDisplacement(X, Y, Z).  
 
filterEnglishDisplacement(X, Y, Z) :-
    Z <> verb,
    +before:dir@displaced@Y,
    xend@Y =< start@X,
    filterEnglishLeftDisplacement(X, Y, Z).

filterEnglishDisplacement(X, Y, _Z) :-
    X <> [word],
    displaced@Y <> xbefore,
    Y <> testWHMarked,
    !.

filterEnglishDisplacement(_X, Y, _Z) :-
    displaced@Y <> xbefore,
    Y <> testWHMarked,
    theta@Y <-> arg(_),
    !.
       
%%%% "CENTRE" SHIFTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This one allows non-compact sentential arguments to be shifted around
% the main verb
% (This is for "Betty, I believe, is a fool" and "Betty is, I believe, a fool")

filterEnglishDisplacement(X, Y, Z) :-
    Z <> verb,
    -compact@Y,
    theta@Y <-> arg(_),
    Y <> [clause],
    testNoWH(Y),
    xstart@Y < xstart@X,
    xend@Y > xend@X.

% For shifting the arguments of "is" in "Betty is, I believe, a fool" 
% and "Betty, I believe, is a fool": two cases, but basically what we're
% looking for is a comma-bounded gap

filterEnglishDisplacement(X, Y, Z) :-
    Z <> verb,
    -aux@Z,
    +after:dir@displaced@Y,
    +compact@Y,
    theta@Y <-> arg(_),
    Y <> testNoWH,
    C1 <> comma3,
    C2 <> comma3,
    start@C1 <-> end@X,
    end@C2 <-> start@Y,
    end@X < start@Y+2,
    complete(C1),
    complete(C2).
    
filterEnglishDisplacement(X, Y, Z) :-
    Z <> verb,
    -aux@Z,
    +before:dir@displaced@Y,
    +compact@Y,
    Y <-> subject@Z,
    theta@Y <-> arg(_),
    C1 <> comma3,
    C2 <> comma3,
    start@C1 <-> end@Y,
    end@C2 <-> start@X,
    end@Y < start@X+2,
    complete(C1),
    complete(C2).

%%%% RIGHT SHIFTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% more elegant program than John, quicker girl than Mary, more peaches than pears
% (but not "more peaches than three", because of the when-clause on "more")

filterEnglishRightDisplacement(_HD, ARG, _RESULT) :-
    cat@ARG <-> than,
    ARG <> saturated.   

% Allow skipping over commas (for hesitations and repairs)

/*
% commented out for non-spoken input
% (even messy input shouldn't have this one)

filterEnglishRightDisplacement(X, Y, Z) :-
    [cat, args]@Z <-> [cat, args]@K,
    M <> comma3,
    xend@M <-> xstart@Y,
    xstart@Y > xend@X,
    complete(M),
    span@Z <-> SPANZ,
    span@K <-> SPANK,
    \+ (either(K), SPANZ is SPANK /\ SPANZ).
*/
    
% Right shift subject of copula (which is the only verb whose subject
% can be right shifted

filterEnglishRightDisplacement(V, Y, _S) :-
    +v:xbar@cat@V,
    theta@Y <-> arg(_).
 
% Otherwise things can ONLY be right shifted over modifiers or particles
% So if you're going to right shift then there'd better be one there
% (this is for 'heavy' argument shift)

filterEnglishRightDisplacement(X, Y, Z) :-
    Z <> verb,
    +compact@Y,
    theta@Y <-> arg(_),
    dtrs@X <-> XDTRS,
    +moved:dir@displaced@RX,
    displaced@RX <> xafter,
    dtrs@Y <-> YDTRS,
    +moved:dir@displaced@RY,
    xstart@Y <-> I,
    xend@Y <-> J,
    end@X <-> K,
    start@M <-> end@X,
    end@M <-> start@Y,
    M <> saturated,
    end@M <-> L,
    +compact@M,
    !,
    % J-I is the length of the shifted item. It's got to be at least 3 words
    % long, and it's got to be at least as long as the distance you shift it
    J-I > 2,
    J-I >= I-K,
    \+ element(RY, YDTRS), 
    \+ element(RX, XDTRS),
    (target@M <> vp; M <> particle),
    complete(M),
    L =< I.    

%%%% Needed for inversion of simple sentences, Allan, 060/06/06
% (how old were you then, have you any problems now)

filterEnglishRightDisplacement(X, Y, Z) :-
    X <> vp,
    modified@X <-> 0,
    +moved:dir@displaced@subject@Z,
    displaced@subject@Z <> xafter,
    theta@Y <-> modifier(_).

%%%% LEFT SHIFTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
% Don't know what this one is for, so commented out 3/5/06
filterEnglishLeftDisplacement(X, Y, Z) :-
    !,
    Z <> verb,
    +compact@Y,
    COMMA <> isComma,
    start@COMMA <-> xend@Y,
    \+ retrieve(_I, COMMA).
*/

% Standard position with auxiliary verbs

filterEnglishLeftDisplacement(X, Y, Z) :-
    Z <> [clause, nonfinite],
    xend@Y < xstart@X,
    +compact@Y,
    -aux@Z,
    subject@Z <-> Y,
    AUX <> [verb, word],
    +aux@AUX,
    end@Y <-> start@AUX,
    end@AUX <-> ENDAUX,
    start@X <-> STARTX,
    !,
    % Check there's an auxiliary
    partial(AUX), ENDAUX =< STARTX.

% Aux-inverted position for WH-marked non-subject args

filterEnglishLeftDisplacement(X, Y, Z) :-
    xend@Y < xstart@X,
    testWHMarked(Y),
    +compact@Y,
    AUX <> [verb, word],
    +aux@AUX,
    agree@X <-> agree@A,
    args@AUX <-> [A],
    head@A <-> head@Z,
    end@Y <-> start@AUX, 
    NP <> [np, nom],
    start@NP <-> end@AUX,
    partial(AUX),
    complete(NP),
    testNoWH(NP),
    +realised@NP.       

/*
Otherwise you can left-shift anything out of anything, so long as you
put it next to an NP and an adjacent verb.

Used to require the verb to be tensed, and then suddenly, for no reason
that I can see, that stopped working on 15/12/09. Mystery.
*/

filterEnglishLeftDisplacement(X, Y, _Z) :-
    +compact@Y,
    %% must be moved PAST the verb
    xend@Y < start@core@X, 
    %% NP is the NP I'm going to look for: must follow the left-shifted
    %% item Y
    xend@Y <-> xstart@NP,
    %% I'm interested in its position relative to the whole thing,
    %% so get its end
    xend@NP <-> xstart@V,
    %% Make sure it is a displaced item that we're thinking about
    NP <> [np, nom],
    %% There'd better be a tensed verb
    V <> [verb],
    %% V <> [verb, tensed_form],
    %% Get the start and end of this verb
    %% Now we actually look for them
    complete(NP),
    testNoWH(NP),
    partial(V),
    true.    
    
% You can left-shift WH-marked subjects so long as there's an intervening NP

filterEnglishLeftDisplacement(X, Y, Z) :-
    %% Z is something verb-like (s, vp, verb, ...)
    xend@Y < xstart@X, % must be moved PAST the verb    Z <> clause,
    +compact@Y,
    -aux@Z,    		% Y is its subject: so it must be that X was a VP and Z is an S
    subject@Z <-> Y,    % The subject has been moved (to the left, because we've done all the right shifts)
    testWHMarked(Y),    	% They have to be left-shifted past some other NP: so we'll describe that other NP
    NP <> [np, nom],
    xstart@NP <-> xend@Y,
    complete(NP),
    xend@NP < xstart@X.   

% You can left-shift WH items any time you like
    
filterEnglishLeftDisplacement(_X, Y, Z) :-
    %% Z is something verb-like (s, vp, verb, ...)
    Z <> verb,
    +compact@Y,
    -aux@Z, 
    testWHMarked(Y).

% Can't tighten up the positions on this one because we need it for
% 'on the bus sat an old man.'
filterEnglishLeftDisplacement(X, Y, _Z) :-
    X <> verb,
    theta@Y <-> modifier(_).

%%%% GERMAN %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Any well-formed German constituent can go left.

filterGermanDisplacement(X, Y, Z) :-
    +before:dir@displaced@Y,
    filterLeftGermanDisplacement(X, Y, Z).
    
filterGermanDisplacement(X, Y, Z) :-
    +after:dir@displaced@Y,
    filterRightGermanDisplacement(X, Y, Z).

filterLeftGermanDisplacement(_X, Y, _Z) :-
    +compact@Y.
    
filterRightGermanDisplacement(_X, Y, _Z) :-
    +compact@Y,
    theta@Y <-> arg(_).
     
show_displaced :-
    -compact@X,
    either(X),
    profile(index@X),
    fail.

%%%% ARABIC %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% We only get to here if (a) we are talking Arabic and (b) Y is displaced.

filterArabicDisplacement(X, Y, Z) :-
    +after:dir@displaced@Y,
    start@Y > end@X,
    +compact@Y,
    +marked@Z,
    filterRightArabicDisplacement(X, Y, Z).
    
filterArabicDisplacement(X, Y, Z) :-
    +before:dir@displaced@Y,
    +compact@Y,
    +marked@Z,
    filterLeftArabicDisplacement(X, Y, Z).

filterLeftArabicDisplacement(X, Y, _) :-
    X <> verb,
    theta@Y <-> arg(_).

filterRightArabicDisplacement(X, Y, _) :-
    X <> verb,
    theta@Y <-> arg(_).

    
%%%% SOMALI %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% We only get to here if (a) we are talking Somali and (b) Y is displaced.

filterSomaliDisplacement(X, Y, Z) :-
    +after:dir@displaced@Y,
    +compact@Y,
    +marked@Z,
    filterRightSomaliDisplacement(X, Y, Z).
    
filterSomaliDisplacement(X, Y, Z) :-
    +before:dir@displaced@Y,
    +compact@Y,
    +marked@Z,
    filterLeftSomaliDisplacement(X, Y, Z).

filterLeftSomaliDisplacement(_, _, _).

filterRightSomaliDisplacement(_, _, _).

%%%% PERSIAN %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% We only get to here if (a) we are talking Persian and (b) Y is displaced.

filterPersianDisplacement(X, Y, Z) :-
    +after:dir@displaced@Y,
    % +compact@Y,
    filterRightPersianDisplacement(X, Y, Z).
    
filterPersianDisplacement(X, Y, Z) :-
    +before:dir@displaced@Y,
    % xend@Y > xstart@X,
    filterLeftPersianDisplacement(X, Y, Z).

filterLeftPersianDisplacement(X, Y, Z) :-
    X <> verb,
    penalise(Y, ((-compact@Y; -compact@Z), testNoWH(Y)), 10).

filterLeftPersianDisplacement(X, _Y, _Z) :-
    text@X <-> ra.

% various experimental versions
filterRightPersianDisplacement(_, _, _).
