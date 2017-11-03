
%%%% DEFAULTS.PL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/***
pre_defaults/1 are added to the specification of something you
want before you go and look for it. Some defaults sometimes have to be
blocked: in that case use a compound feature with value \& default
constituents.
*/

pre_defaults(X) :-
    basic_pre_defaults(X),
    def_acc(X).

def_acc(X) :-
    caseDefault@X <-> DC,
    \+ \+ DC = +,
    X <> acc,
    !.
def_acc(_).

basic_pre_defaults(X) :-
    language@X <-> LANGUAGE,
    (fixed@X == + ->
	true;
     LANGUAGE <> persian ->
    	default(dir@X <> xbefore);
     LANGUAGE <> somali ->
    	default(dir@X <> xbefore);
    	default(dir@X <> xafter)),
    % This requires any Somali argument to be focal:
    % if it turns that this only holds for verbs, we will
    % have to set it in vtypes
    % ((language@X <> somali, theta@X <-> arg(_)) -> argument@X <-> *(_); true),
    %%after:dir@displaced@X <-> RDISP,
    %% default(RDISP = -),
    %%bracketed@X <-> B,
    %% default(B = -),
    ellipse@X <-> E, 
    default(E = -),
    xlist@X <-> XLIST,
    default(XLIST = -),
    % (LANGUAGE <> english -> default(-after:dir@displaced@X); true),
    usedef:def@zero@X <-> ZERO_D,
    value:def@zero@X <-> ZERO_V,
    (LANGUAGE <> somali ->
	true;
	default((\+ \+ ZERO_D = +, ZERO_V = -))).

defaultClitic(LANGUAGE, X) :-
    clitic@X <-> CLITIC,
    X <> nominal,
    LANGUAGE <> arabic,
    !,
    default(CLITIC = -).

defaultClitic(LANGUAGE, X) :-
    clitic@X <-> CLITIC,
    LANGUAGE <> somali,
    X <> nominal,
    !,
    default(CLITIC = -),
    default(def_acc(X)).

defaultClitic(_, _).

/***
post_defaults/1 add information to complete edges once you've
made them.  For instance, things are not case-marked in English unless
they are.

\medpara
Note the test for complete non-specification for ACC. Needed to block
application of this default when conjoining subject NPs.
*/

post_defaults(X) :-
    language@X <-> LANGUAGE,
    cat@X <-> CAT,
    specifier@X <-> SPEC,
    indefinite@X <-> INDEF,
    ((LANGUAGE <> arabic, \+ \+ +n:xbar@CAT, -pcase@X) ->
        (default(INDEF = +), default(SPEC <-> existential(_)));
        true),
    defaultClitic(LANGUAGE, X),
    default(not_case_marked(X)),
    type@X <-> _TYPE,
    target@X <-> TARGET,
    cat@TARGET <-> TCAT,
    index@TARGET <-> TARGETINDEX,
    (var(dtrs@TARGET) ->
	default(TCAT = -);
	true),
    (TCAT == - -> TARGETINDEX = -; true),
    predicative@X <-> PRED,
    default(PRED = -),
    bracketed@X <-> B,
    default(B = -),
    % realised@X <-> REAL,
    % default(REAL = +),
    conj@X <-> CONJ,
    default(CONJ = -),
    usedef:def@generic@X <-> DGEN,
    value:def@generic@X <-> VGEN,
    default((\+ \+ noun(X), \+ \+ DGEN = (+), VGEN = (-))),
    % modification makes them real - otherwise, they're irreal(?) (Helen, 31/07/01)
    % irreal@X <-> IRREAL,
    % out because it interferes with generation! AR, 28/06/07
    % default((\+ \+ np(X), IRREAL = (+))),
    % default((\+ \+ pp(X), IRREAL = (+))),
    topicalised@X <-> TOPIC,
    default(TOPIC = []),
    default(no_case_default(X)),
    default(set_direction(X)),
    value:def@zero@X <-> ZERO,
    default(ZERO = -),
    modifierX <-> MODIFIED,
    default(MODIFIED = 0).
