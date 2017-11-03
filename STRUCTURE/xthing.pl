/*
	XTHING.PL
	
	Stuff for dealing with "anything", "something", "nothing"
*/


xthing(X) :-
    X <> [np, inflected, third, singular],
    cat@X <-> [_NOM, noun, pronoun(-) | _],
    semantics@X <-> xthing,
    -date@X.
    

xthing(X) :-
    X <> [inflected, third, singular, noun],
    cat@X <-> [_NOM, noun, pronoun(-) | _],
    +specified@X,
    +fixed@X,
    args@X <-> [MOD],
    theta@MOD <-> arg(identity),
    target@MOD <> noun,
    semantics@X <-> xthing,
    -date@X.
