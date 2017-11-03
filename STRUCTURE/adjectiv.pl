
/*
ADJECTIVES.PL

Sets various properties of an item depending on whether it is being 
viewed as an adjective or an adverb --- notably what it target (!) and
whether or not it is predicative (i.e. capable of being the object of
a copula verb).

These procedures should be called by whatever deals with the morphology
of adjectives, and hence this is where things like comparatives should
be defined.

*/

adj_set(A) :-
    %% set to pcase to allow it to be the argument of 'keep': 03-12-09
    A <> [adj, adjunct, pcase],
    %% definiteness included for Arabic, doesn't hurt anytjhing else
    [agree, def]@A <-> [agree, def]@target@A,
    target@A <> noun, 
    nonfoot@target@A <-> nonfoot@result@A,
    %% Using the 2-part version of specified (this could screw up for
    %% Persian: have to test it)
    +unspecified@target@A,
    % -moved@displaced@A,
    default(modifier@A <-> *adjmod),
    predicative@A <-> *(adjpred),
    ptag:tag@tag@A <> tag(adjective).

adv_set(A) :-
    A <> [adj, adjunct],
    +v:xbar@cat@target@A,
    nonfoot@target@A <-> nonfoot@result@A,
    default(modifier@A <-> *advmod(n:xbar@cat@target@A)),
    trigger(index@target@A, (target_of_adverb(target@A), penalise(target@A, -6))),
    ptag:tag@tag@A <> tag(adverb).

target_of_adverb(P) :-
    P <> adj,
    dir@P <> xbefore.
target_of_adverb(P) :-
    (language@P <> english ->
	P <> vp;
	P <> verb).

softArabicAdj(X) :-
    TARGET <-> target@X,
    gender@X <-> GENDERX,
    gender@TARGET <-> GENDERT,
    softParse(GENDERT,
              GENDERX = GENDERT,
              wrongGenderForAdj(index@X, index@TARGET),
              X),
    def@X <-> DEFX,
    def@TARGET <-> DEFT,
    softParse(DEFT,
              DEFX = DEFT,
              wrongDefForAdj(index@X, index@TARGET),
              X),
    third@X <-> AGRX,
    third@TARGET <-> AGRT,
    softParse(AGRT,
              AGRX = AGRT,
              wrongAgrForAdj(index@X, index@TARGET),
              X),
    TARGET <> nominal,
    +fixed@TARGET,
    softParse(before:dir@dir@TARGET,
              before:dir@dir@TARGET == -,
              wrongDirForTargetOfAdj(index@X, index@TARGET),
              X),
    X <> adjunct,
    specifier@X <-> identity,
    modifier@X <-> *identity.
