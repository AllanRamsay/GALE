 /*
 	VERBS.PL
  	Convert the argument sets specified in vtypes.pl into subcat lists
  	Some actions will depend on the language: the aim is to restrict these
 	to the partial orderings, but we'll see how it goes

  */  

% Optionally delete optional items  

select(V, args@V) :-
     args@V <-> A1,
     select(V, A1, _ARGS).

select(_V, [], _L). 
select(V, [H | T], S1) :-
     language@V <-> language@H,
     % default(-optional@H),
     select(V, T, S0),
     optional@H <-> OPT,
     value:def@zero@H <-> VZERO,
     (S1 = [H | S0];
      (/*+optional@H*/ nonvar(OPT), VZERO = -, S1 = S0)).  

% Mark all but the first N arguments as optional

set_valency(_, [], []). 
set_valency(I, [H0 | T0], T2) :-
     !,
     (atom(H0) -> (theta@H1 <-> H0, default(H1 <> np)); H1 = H0),
     ((I < 1, +optional@H1, T2 = T1);
      T2 = [H1 | T1]),
     set_valency(I-1, T0, T1).  

valency(V, N, A) :-
     set_valency(N, A, args@V).  
% Delete optional arguments if you want to; choose a subject from what's 
% left; and decide which argument you want to look for first.  

set_args(X, A, X) :-
    fixed@X <-> FIXED,
     \+ FIXED = -,
     A <-> args@X,
     !.  
set_args(V, ARGS, V) :-
     \+ \+ var(theta@subject@V),
      \+ \+ -comp@V,
     V <> verb,
     -aux@V,
     not_case_marked(V),
     !,
     positions(V, ARGS).  
set_args(X, A, X) :-
    args@X <-> A.
 
