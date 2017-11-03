
%%%% MODIFIER.PL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*******************************************************************
\medpara
Stuff for combining a modifier and a target. The crucial predicate is
combineModAndTarget/4, but there are a lot of smaller predicates
which mainly either say something about the order of the two items or
describe what kinds of things can modify one another. One complication
here: I can never recmember whether I use \texttt{dir@target@X <>
xbefore} to mean the target is before the modifier or the modifier is
before the target. The definitive answer is that \texttt{dir@target@X
<> xbefore} means the modifier precedes the target: this may be
counter-intuitive, but it's what I've gone for and it's too late to
change it now.

\medpara
Ordering rules: in English, there is a general rule that modifiers
precede their targets if the core word is the last word in the phrase
and follow them if not, so you get "happy man", "sleeping man",
"quietly sleeping man" but "man happy in his work", "man eating a
peach", "man sleeping in the park". For other languages I currently
just assume a default direction for modifiers. In all cases setting
\texttt{+fixed@target@X} overrides the rules below, so if you have an
exception (\eg English \q{ago}) then you do \texttt{+fixed@target@X}
and set the direction explicitly.

\medpara
What can modify what: all fairly self-explanatory.

\medpara
combineModAndTarget/4 has, like combinePartialAndComplete/2,
a pile of preliminaries for extracting features from the input edges
\& describing the result; a couple of lines in the middle for actually
looking for the target or modifier, as appropriate; stuff for
computing the position of the result, and for checking movement and
compactness; and then a call to filter/3. It is important to note
that we use combineModAndTarget/4 for true modifiers (which we
call \scare{adjuncts}), which share the value of \feat{spec}, and
specifiers. The result of combining a modifier and its target is
described in the \feat{result} feature of the modifier.
*******************************************************************/

%%%% ORDERING RULES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/***
Is the key word the last word in the phrase? (needed for English ?German?)
*/

check_ends(X) :-
    testNoWH(X),
    end@X <-> end@core@X.
check_ends(X) :-
    xend@X is xstart@X+1.

one_word_vp_modifier(X) :-
     text@X <-> TEXT,
     \+ TEXT = [_ | _],
     target@X <> proper_vp. 
      
set_direction(X) :-
     (fixed@target@X == (+)),
     !.  

set_direction(X) :-
     var(core@X),
     !.  

set_direction(X) :-
     -target@X,
     !.  

set_direction(X) :-
     language@X <> spanish,
     !.  
     
set_direction(X) :-
     language@X <> arabic,
     default(dir@target@X <> xafter).  

set_direction(X) :-
     language@X <> french,
     !,
     default(-after:dir@dir@target@X).  

set_direction(X) :-
     \+ -bracketed@X,
     !.  

set_direction(X) :-
     \+ -conj@X,
     !.  

set_direction(X) :-
     dir@target@X <-> D,
     core@X <-> CORE,
     language@X <> english,
     !,
     (var(CORE) ->
       	true;
       	(check_ends(X) ->
       	    D <> xbefore;
 	    D <> xafter)).  

set_direction(X) :-
     CORE <-> core@X,
     !,
     (var(CORE) ->
	 true;
	 default(-after:dir@dir@target@X)).

%%%% WHAT CAN MODIFY WHAT: target may be a type or an unevaluated constraint

modifiable(X, MOD) :-
    X <> word,
    comma1(MOD).

modifiable(X, MOD) :-
    X <> word,
    cat@MOD <-> focus.

modifiable(X, MOD) :-
    %% determiners are unsaturated nominals (for today) and
    %% are modifiable (in general), 11/11/09
    %% X <> [saturated],
    +n:xbar@cat@X,
    -moved:dir@displaced@MOD,
    +compact@result@MOD,
    %% disallow bracketed items as noun modifiers, 24/11
    -bracketed@MOD.  

modifiable(X, _MOD) :-
    language@X <-> L,
    -spanish:lang@L,
    -greek:lang@L,
    -german:lang@L,
     X <> verbal.  

modifiable(X, _MOD) :-
    language@X <-> L,
    -spanish:lang@L,
    -greek:lang@L,
    -german:lang@L,
     X <> adj,
     -aux@X.  

% Somali topics modify sentence AFTER they have picked up their complementisers
modifiable(X, _MOD) :-
     (language@X <> somali ->
	 true;
	 -comp@X),
     X <> s,
     -aux@X.

modifiable(X, _MOD) :-
     X <> punct.

modifiable(_, _).

noun_or_vp(X) :-
     X <> [nominal, saturated],
     (language@X <> english ->
	 +unspecified@X;
	 true).

noun_or_vp(X) :-
     X <> [vp, not_to_form],
     -aux@X.  

target_of_rclause(T, R) :-
     %% dir@T <> xafter,
     %% kspec@T <-> kspec@R,
    case@T <-> case@R,
    noun_or_vp(T).
target_of_rclause(T, R) :-
    language@R <> somali,
    T <> s,
    -comp@T.

target_of_nominal(TARGET) :-
     TARGET <> [noun, saturated],
     +unspecified@TARGET. 

target_of_nominal(TARGET) :-
    -aux@TARGET,
    TARGET <> s, 
    -before:dir@dir@TARGET.  

target_of_gerund(SORT, TARGET) :-
    checkType(TARGET, SORT),
    TARGET <> [noun, saturated],
    +unspecified@TARGET.

target_of_gerund(_SORT, TARGET) :-
    -aux@TARGET,
    TARGET <> vp.

/***
X and Y will combine if Y is an appropriate modifier for X and appears
to its left or right, as specified by its own mod_direction. Z is
whichever is not known. The only real complication is that we want to
call set_direction early, but in that case if Y is the unknown one
then we don't want to call a default rule on it. So set_direction is
blocked from doing anything to unknowns.

There is never anything to the right of where you are. So if it's the
target that's missing then you know that you're only going to find it
by looking left, and hence what you've got must be a right
modifier. Can't afford to use the same reasoning to deduce that only
left modifiers can be missing because it could actually be a
left-extracted right modifier.
*/

combineModAndTarget(X, Y, Z, S) :-
    %% Extract all the fields
    startTimer(combineModAndTarget),
    +modifiable@X,
    start@X <-> I1,
    end@X <-> J1,
    start@Y <-> I2,
    end@Y <-> J2,
    +realised@X,
    +realised@Y,
    theta@Y <-> modifier(modifier@Y),
    topicalised@Y <-> YTOPIC,
    start@S <-> I3,
    end@S <-> J3,
    -ellipse@X,
    [cat, agree, mcopy, args, language, type, semantics, fixed, conj, sort]@S
  	<-> [cat, agree, mcopy, args, language, type, semantics, fixed, conj, sort]@X,
     [predicative, nonfoot, modified, index, compact, mod, intensified, uses, score, focus]@S
      <-> [predicative, nonfoot, modified, index, compact, mod, intensified, uses, score, focus]@result@Y,
     core@X <-> core@S,
     modified@S <-> YMODIFIED,
     modified@X <-> XMODIFIED,
     span@X <-> SPAN1,
     span@Y <-> SPAN2,
     span@S <-> SPAN3,
     xstart@X <-> XS1,
     xstart@Y <-> XS2,
     xstart@S <-> XS3,
     xend@X <-> XE1,
     xend@Y <-> XE2,
     xend@S <-> XE3,
     dtrs@X <-> XDTRS,
     compact@S <-> COMPACT,
     displaced@Y <-> DISP,
     moved:dir@DISP <-> MOVED,
     before:dir@DISP <-> BDISP,
     after:dir@DISP <-> ADISP,
     dir@target@Y <-> DIR,
     %% dir@target@DTR <-> dir@target@Y,
     %% modified@result@DTR <-> modified@result@Y,
     before:dir@DIR <-> LDIR,
     after:dir@DIR <-> RDIR,
     %% RMOD is a dummy right modifier, to be used when checking that we are
     %% not left modifying something that already has a right modifier
     theta@RMOD <-> modifier(_),
     modified@result@RMOD <-> modified@result@Y,
     dir@target@RMOD <> xafter,
     %% likewise DMOD, for looking for existing displaced modifiers
     theta@DMOD <-> modifier(_),
     +moved:dir@displaced@DMOD,
     %% I used to share just roles, for some unknown reason. To make Helen's
     %% treatment of "because" work we need to share meanings.
     [index, compact, positions, wh, displaced, meaning, case, clitic, cat]\sort@Y
  	<-> [index, compact, positions, wh, displaced, meaning, case, clitic, cat]\sort@DTR,
     dir@target@X <-> dir@DTR,
     %% We have to do this afterwards, so why do it before as well? (28/01/03)
     %% (because it speeds it up! 16/12/06)
     \+ \+ modifiable(X, Y),
     (var(SPAN1) ->
	 trigger(SPAN1, 0 is SPAN1 /\ SPAN2);
	 trigger(SPAN2, 0 is SPAN1 /\ SPAN2)),
     (MOVED == - ->
	 (LDIR == - -> J1 = I2;
	     RDIR == - -> I1 = J2;
	     true);
	 true),
     Z,
     0 is SPAN1 /\ SPAN2,
     \+ \+ modifiable(X, Y),
     dtrs@S <-> [DTR, prev(index@X) | DTRS],
     DTRS <-> dtrs@Q,
     [args,index]@Q <-> [args,index]@X,
     ((staticDtrs; XDTRS = []) ->
	 DTRS = XDTRS;
	 trigger(DTRS, either(Q))),
     default(XMODIFIED = 0),
     default(YMODIFIED = 0),
     (((I1 = J2, LDIR = +); (J1 = I2, RDIR = +)) ->
	 (MOVED = -);
	 %% If a modifier is shifted it must go left
	 %% (now allowing them to go right as well, but I can't
	 %% remember the cases where it's needed: Allan, 06/06/06)
	 ((I2 < I1 -> 
	   ( %% XS2 < XS1,  % taken out for German: is this dangerous?
	       BDISP = +,
	       (J2 = I1 ->
		   penalise(X, 10);
		   true));
	   ADISP = +), 
	     MOVED = +,
	     YTOPIC <-> [])),
     (YMODIFIED > XMODIFIED ->
	 true;
	 (YMODIFIED = XMODIFIED, 
	     (I2 < I1 ->
		 (%% force it to attach left-shifted ones first if they're equal
		    MOVED = -, I2 < XS1);
		 true))),
     min(XS1, XS2, XS3),
     max(XE1, XE2, XE3),
     SPAN3 is SPAN1 \/ SPAN2,
     countZeros(SPAN3, XS3, ZEROS, +),
     (ZEROS == 0 ->
 	  (COMPACT = +, I3 = XS3, J3 = XE3);
 	  (COMPACT = -,
	      %% (ZEROS < 2 -> true; penalise(X, 10)),
	      (MOVED = - ->
		  (min(I1, I2, I3), max(J1, J2, J3));
		  (I3 = I1, J3 = J1)))),
     stopTimer(combineModAndTarget),
     failures@S <-> failures@X+failures@Y,
     get_mutable(SCOREX, score@X),
     get_mutable(SCOREY, score@Y),
     SCORE is SCOREX+SCOREY+3,
     penalise(S, SCORE),
     filter(X, Y, S). 
