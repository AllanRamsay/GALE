
%%%% LEXABBREVIATIONS.PL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% used in standardentries

translation(X, translation@X).
semantics(X, semantics@X).
sense(X, sense@X).

noun(X, generic) :-
    -usedef:def@generic@X,
    X <> nominal,
    trigger(text@X, default(create_mutable(text@X, sort@X))).
    
noun(X, plain) :-
    X <> nominal,
    trigger(text@X, default(create_mutable(text@X, sort@X))).

intransPrep(X) :-
    preposition(X, intransitive, text@X).

transPrep(X) :-
    preposition(X, npcomp, text@X).

iverb(X) :-
    vtype(X, valency(1, [agent])).

itverb(X) :-
    vtype(X, valency(1, [agent, object])).

tverb(X) :-
    vtype(X, valency(2, [agent, object])).

sverb(X) :-
    vtype(X, valency(2, [agent, EVENT])),
    EVENT <> [s, scomp, tensed_form].

vverb(X, N, ARGS) :-
    vtype(X, valency(N, ARGS)).

adverb(X) :-
    X <> [adj, adv_set, inflected],
    semantics@X <-> text@X.

number(X, N) :-
    X <> inflected,
    make_number(N, X).

setSort(X, S) :-
    create_mutable(S, sort@X).