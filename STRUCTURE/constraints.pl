
%%%% CONSTRAINTS.PL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

addFailure(X, N, F) :-
    penalise(X, N),
    extendWithX(F, failures@X).

/***
Package up when/2 clauses so that we can set a constraint and impose a penalty
if it's violated
***/

softParse(TRIGGER, TEST, COST, DIAGNOSIS, X) :-
    [remarks, score]@X <-> [remarks, score]@Y,
    trigger(TRIGGER, doSoftParse(TEST, COST, DIAGNOSIS, X)).

softParse(TRIGGER, TEST, DIAGNOSIS, X) :-
    [remarks, score]@X <-> [remarks, score]@Y,
    trigger(TRIGGER, doSoftParse(TEST, DIAGNOSIS, Y)).

doSoftParse(TEST, DIAGNOSIS, Y) :-
    (TEST ->
	true;
	(soft(DIAGNOSIS, COST),
	    addFailure(Y, COST, DIAGNOSIS))).

doSoftParse(TEST, COST, DIAGNOSIS, Y) :-
    (TEST ->
	true;
	(soft(DIAGNOSIS, _COST),
	    (COST > 0 -> addFailure(Y, COST, DIAGNOSIS); true))).
