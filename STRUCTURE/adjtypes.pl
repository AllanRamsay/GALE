


/*
	ADJTYPES.PL

	List of basic adjective types.

	Note that nothing here says anything about what an adjective can
	modify. Over in "adjectives.pl" two procedures --- adj_set and adv_set
	--- specify the properties of adjectives and adverbs, and in suffixes
	there are rules which say what adjectives with specific suffixes can
	modify. 
	
  	Partially merged with ~/VERSIONS/VAHID, 19/4/00
*/

adjtype(A, simple, semantics@A) :-
    (language@A <> persian ->
        default(dir@target@A <> xafter);
        true),
    A <> [adj1, intransitive],
    %modified@result@A <-> 4,
    % Set to 2 to allow adverbs to precede PPs, 21/11/00
    modified@result@A <-> 2.

adjtype(A, simple) :-
    A <> adj,
    adjtype(A, simple, adj(text@A, theta@A, degree@A, intersective@A)).

adjtype(A, intersective) :-
    adjtype(A, simple),
    +intersective@A.

adjtype(A, factive) :-
    adjtype(A, simple),
    intersective@A <-> factive.

adjtype(A, nonfactive) :-
    adjtype(A, simple),
    -intersective@A.

adjtype(A, sort(S)) :-
    A <> adj,
    adjtype(A, simple, adj(sort(S), theta@A, degree@A, intersective@A)).
adjtype(A, scalar(S)) :-
    A <> adj,
    adjtype(A, simple, adj(sort(S), theta@A, degree@A, intersective@A)).

adjtype(A, so) :-
    A <> adj, 
    adj_set(A),
    semantics@A <-> lambda(P, lambda(X, #[text@A, X, P])),
    args@A <-> [ADJ, S],
    S <> [s, testNoWH],
    ADJ <> [adj, intransitive].

adjtype(A, smodifier) :-
    A <> [adj, intransitive],
    -before:dir@dir@target@A,
    target@A <> s, 
    semantics@A <-> lambda(P, quant(focus, lambda(W, #[text@A, W]), P)).

adjtype(A, focal(F)) :-
    A <> [intransitive, adj1],
    modified@result@A <-> 3.5,
    target@A <> verb,
    modifier@A <-> *identity,
    semantics@A <-> F.

adjtype(A, only) :-
    adjtype(A, focal(only)),
    modified@result@A <-> 3.5.
    
adjtype(A, even) :-
    adjtype(A, focal(lambda(I, lambda(J, qq(0.00055, even(I:J)))))).
    
adjtype(A, eager) :-
    A <> adj,
    args@A <-> [VP],
    modified@result@A <-> 2,
    % Force this kind of adj to accept TO-form VPs
    VP <> [vp, to_infinitive_form].

/*
adjtype(A, not) :-
    adjtype(A, focal(lambda(I, qq(0.005, lambda(J, qq(0.0055, not(I:J))))))).
*/

adjtype(A, not) :-
    A <> [adj1, intransitive],
    % Helen, 31/07/01 (copied from dictionary)
    -irreal@result@A,
    modified@result@A <-> 0.2,
    target@A <-> TARGET,
    %%%% Using WHEN rather than COMPLETION (merging at 31/7/01)
    % when(nonvar(index@TARGET), not_completion(TARGET)),
    % Fill in the category a bit, largely to cope with the change
    % from -target@X to -cat@target@X as the default
    TARGET <> [x, nonfinite, word],
    modifier@A <-> *not,
    %%%% Semantics specified by setting polarity (merging at 31/7/01)
    %semantics@A <-> negate,
    semantics@A <-> identity,
    %polarity@result@A <-> lambda(I, qq(2.1, not(I))).
    true. %% polarity@result@A <-> negated.


/*
adjtype(A, not) :-
    adjtype(A, focal(not)).
*/

adjtype(A, intensifier) :-
    modified@A <-> -1,
    A <> [adj, saturated],
    target@A <> [adj, saturated].

adjtype(A, due) :-
    A <> adj,
    args@A <-> [PP], 
    target@A <> [intransitive, noun],
    -after:dir@dir@target@A, 
    PP <> [pp],
    pcase(PP, to).

% New class of adj (with apparently no example in the dictionary!)
adjtype(A, scomp) :-
    A <> adj,
    args@A <-> [S], 
    target@A <> [intransitive, noun],
    -after:dir@dir@target@A,
    S <> s,
    +comp@S.

not_completion(TARGET) :-
    TARGET <> [vp, nonfinite].
not_completion(TARGET) :-
    TARGET <> np.
not_completion(TARGET) :-
    TARGET <> adj.
