%%%% DETERMINERS.PL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/**************************************************************************

\medpara
Two things matter about a determiner: what complements it takes, and
what it means. dtype/3 specifies various bits of syntactic
information, notably the complement type, and then hands over to
det_meaning/2 to fix the meaning. I change my mind on a regular but
infrequent basis about whether I think determiners are unsaturated NPs
(the categorial view) or specifiers (the HPSG) view. I do this every
couple of years or so, so I tend to forget why I made the choice I did
last time I changed, and I have difficulty tracking down all the
places that need to be changed. I have finally tried to tidy this up
so that there are two predicates, dtarget/2 and dresult/2, defined in
STRUCTURE/classes.pl, which currently pick out the
\scare{target} and \scare{result} but which can easily be changed to
give the categorial version instead.

\medpara
The general pattern in det_meaning/2
is that you get something like

 	    \verb%lambda(P, exists(S, subset(S, man) & |S|=1 & P.S))%

\medpara
for the NP "a man".
   
\medpara     
The only serious complication is that the quant of the quantifier is
left unresolved (see e.g. "The Core Language Engine", \citep{Alshawi:92}
for similar stuff). So \q{a man} comes out more like

  	\verb%quant("a", lambda(W, exists(S::{subset(S,man) & |S|=1}, W), S)%

\medpara
This says that the term S is to be used where the
meaning of "a man" is required, and that the bigger abstraction is to
plant a quantifier round some formula (which will contain the other
one, and hence will contain a free occurrence of S) when you've worked
out all the relevant scope preferences. The lexical item is retained
as the key to deciding on scope preferences.    
  
************************************************************************/

dtype(X, DREF) :-
     dtype(X, simple, DREF).

dtype(X, the, DREF):-
     dresult(X, RESULT),
     +definite@RESULT,
     X <> [det, spec],
     type@X <-> det(the),
     default(modified@X <-> -1),
     dtype(X, simple2(_NN), DREF).  

dtype(X, simple(NN), DREF) :-
     X <> det,
     default(modified@X <-> -1),
     NN <> fullyUnspecified,
     dtype(X, simple1(NN), DREF),
     -moved:dir@displaced@NN. 
 
dtype(X, compound(NN, PREP), DREF) :-
     X <> det1,
     pcase(X, PREP),
     dtype(X, simple1(NN), DREF).  

dtype(X, all, DREF) :-
     X <> det,
     type@X <-> det(all),
     default(modified@X <-> -1),
     % If it doesn't work, try taking this out
     result@X <> fullySpecified,
     modifier@X <-> *identity,
     dtarget(X, TARGET),
     dresult(X, RESULT),
     card@TARGET <-> card@RESULT,
     definite@RESULT <-> definite@TARGET,
     specifier@RESULT <-> {universal, all},
     dtype(X, simple4(TARGET), DREF).  

dtype(X, both, DREF) :-
     X <> det,
     type@X <-> det(both),
     default(modified@X <-> -1),
     result@X <> fullySpecified,
     modifier@X <-> *identity,
     card@result@X <-> 2,
     +definite@result@X,
     dtype(X,simple3(_NN), DREF).  

% the number may or may not act as a specifier 
dtype(X, number, number(NUM)) :-
     X <> det,
     dtarget(X, NN),
     type@X <-> number,
     default(modified@X <-> -1),
	% If it doesn't work, try taking this out
     %% NN <> fullyUnspecified,
     modifier@X <-> numAsMod(NUM),
     semantics@X <-> NUM,
     arg@X <-> identity,
     -value:def@generic@NN,
     NN <> [nominal, saturated],
     +unspecified@NN,
     %% -moved:dir@displaced@NN,
     no_case_default(NN),
     date@X <-> date@NN,
     agree@X <-> agree@NN,
     dresult(X, RESULT),
     predicative@RESULT <-> *nppred,
     specifier@RESULT <-> number,
     [date, card]@NN <-> [date, card]@RESULT,
     trigger(index@NN, nounOrOf(NN)).

nounOrOf(X) :-
    X <> fullyUnspecified,
    -moved:dir@displaced@X.
nounOrOf(X) :-
    X <> fullySpecified,
    pcase(X, of).

dtype(X, simple1(NN), DREF) :-
    %% This is where typical determiners say they produce an NP from
    %% an NN    
    X <> spec,	 
    dtype(X, simple2(NN), DREF).  

dtype(X, simple2(NN), DREF) :-
    dresult(X, RESULT),
    modifier@X <-> *identity,
    [date, card]@NN <-> [date, card]@RESULT,
    dtype(X, simple3(NN), DREF).  

dtype(X, simple3(NN), DREF) :-
    dresult(X, RESULT),
    specifier@RESULT <-> DREF,
    dtype(X, simple4(NN), DREF).  

dtype(X, simple4(NN), DREF) :-
    dtype(X, simple5(NN), DREF).

dtype(X, simple5(NN), _DREF) :-
    dresult(X, RESULT),
    dtarget(X, NN),
    -value:def@generic@NN,
    NN <> [nominal, saturated],
    -moved:dir@displaced@NN,
    no_case_default(NN),
    %% case@X <-> case@NN,
    agree@X <-> agree@NN,
    default(semantics@X <-> identity),
    predicative@RESULT <-> *nppred,
    default(tag(ptag:tag@tag@X, det)).  

dtype(X, simple, DREF) :-
    dtype(X, simple(_), DREF).

dtype(X, of, DREF) :-
    pcase(NP, of),
    NP <> np,
    dtype(X, np(NP), DREF).  

dtype(X, np, DREF) :-
    NP <> [np, acc],
    dtype(X, np(NP), DREF).  

dtype(X, np(NP), DREF) :-
    X <> [det, spec],
    args@X <-> [NP],
    -moved:dir@displaced@NP,
    -modifier@X,
    semantics@X <-> det_meaning(DREF, index@X, lambda(I, lambda(J, I=J))).  

numeral(X) :-
     %% Malay classifiers, not numbers!
     X <> det,
     args@X <-> [C, N],
     agree@X <-> agree@C,
     agree@X <-> agree@N,
     classifier(C),
     N <> noun.  

classifier(A) :-
     A <> saturated,
     -moved:dir@displaced@A,
     cat@A <-> classifier,
     text@A <-> kind@A.  

restrictor(NN, AGREE, R) :-
     (flag(individuals) ->
 	R = NN;
 	R = lambda(S, card(S, AGREE) & subset(S, NN))).  


/*** 

Indefinite determiner is going to help construct an NP.  The meaning
of an NP will be something with a hole in it, because we will be using
it up later on inside a VP or a PP or something like that, and that
will supply part of the meaning.  In order to construct the meaning of
the NP, the determiner will need to be supplied with two pieces of
information, namely the meaning of the NN and the agreement of the
whole thing (so we can tell whether it's singular or plural).  This is
fairly Montague-like.  There is, however, a complication.  Where
Montague would produce \texttt{lambda(Q, exists(X, man(X) \& Q.X))}
for "a man", I'm going to produce \texttt{lambda(P, qq("a", exists(X
:: \{man(X)\}, P.X))}.  Why?  Because I don't know, in the sentence
"every man loves a woman", whether "every man" or "some woman"
dominates the sentence.  What we do is sort out the scopes much later
on, using facts about specific lexical items to help us see which one
should have wider scope.  \texttt{exists(S :: \{R:S\}, Q:S)} = "for
some S such that R holds of S, Q also holds of S" What we're doing
here is called in-situ quantification or quasi-logical form: the
initial translation doesn't determine the scope of the quantifiers, so
we leave them wrapped up in a term marked by qq (for quantifier) and
sort it out later.  Ref: quasi-logical form is described in "The CORE
language engine", \cite{Alshawi:92}.

*/

det_meaning(X, which) :-
     !,
     -definite@X,
     restrictor(NN, AGREE, R),
     semantics@X <->
   	  lambda(AGREE,
  	     lambda(NN,
              lambda(P,
  		    qq(text@X, query(S :: {R:S}, (P:S)))))).  

det_meaning(X, personal(OWNER, _CARD)) :-
     !,
     +definite@X,
     restrictor(NN, AGREE, R),
     semantics@X <->
     lambda(AGREE,
  	   lambda(NN,
		  lambda(P,
			 qq(the,
			    ref(O :: {(OWNER)},
				qq(the, ref(S::{(R) & owns(O, S)}, P:S))))))).   

det_meaning(X, indefinite) :-
     !,
     -definite@X,
     restrictor(NN, AGREE, R),
     semantics@X <->
   	  lambda(AGREE,
  	     lambda(NN,
              lambda(P, qq(text@X, exists(S :: {R:S}, P:S))))).  

det_meaning(X, any) :-
     !,
     -definite@X,
     restrictor(NN, AGREE, R),
     semantics@X <->
   	  lambda(AGREE,
  	     lambda(NN,
              lambda(P, qq(text@X, any(S :: R, P:S))))). 

 det_meaning(X, universal) :-
     !,
     % This one's the norm
     -definite@X,
     restrictor(NN, AGREE, R),
     semantics@X <->
   	  lambda(AGREE,
  	     lambda(NN,
              lambda(P, qq(text@X, forall(S :: subset(S, R), P:S))))). 

det_meaning(X, definite) :-
     !,
     +definite@X,
     restrictor(NN, AGREE, R),
     semantics@X <->
   	lambda(AGREE,
  	   lambda(NN,
            lambda(P, qq(text@X, ref(S :: {R}, P:S))))).  

det_meaning(X, denial) :-
     !,
     -definite@X,
     restrictor(NN, AGREE, R),

     semantics@X <->
   	  lambda(AGREE,
  	     lambda(NN,
              lambda(P, qq(text@X, no(S::{R:S}, (P:S)))))).  

det_meaning(X, number(_NUM)) :-
     !,
     det_meaning(X, indefinite).  

det_meaning(X, number(_NUM)) :-
     !,
     -definite@X,
     semantics@X <->
   	  lambda(_AGREE,
  	     lambda(NN,
              lambda(P, qq(text@X, exists(S :: {NN}, (P:S)))))).  

make_number(N, X) :-
     (N = 1 ->
          third_sing(X);
          third_plural(X)),
     ptag:tag@tag@X <> tag(number),
     dtype(X, number, number(N)).

make_number(_N, X) :-
    X <> [noun, fullySpecified, inflected],
    args@X <-> [NP],
    NP <> [np, notnom, pcase(of)],
    date@X <-> date@NP.
    
