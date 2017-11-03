%%% SOUNDS.PL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
onset(X) :-
	S <-> syllpos:char@X,
	+initial:pos@S,
	-middle:pos@S,
	-final:pos@S.

nucleus(X) :-
	-initial:pos@syllpos:char@X,
	+middle:pos@syllpos:char@X,
	-final:pos@syllpos:char@X.

coda(X) :-
	-initial:pos@syllpos:char@X,
	-middle:pos@syllpos:char@X,
	+final:pos@syllpos:char@X.

 %%%%% Vowels features %%%%%%%

vowel(X) :-
 	X <> [+vowel]:char.

consonant(X) :-
 	X <> [-vowel]:char.

longV(X) :-
 	X <> [vowel, +long]:char.

shortV(X) :-
 	X <> [vowel, -long]:char.

roundV(X) :-
 	X <> [vowel, +rounded]:char.

unroundV(X):-
 	X <> [vowel,-rounded]:char.

openV(X):-
 	X <> [vowel, +open]:char.

closeV(X):-
 	X <> [vowel, -open]:char.

dash(X):-
	X <> [-vowel, +dash]:char.

 %%%%% Consonant features%%%%%%%%%
 %%%% Manner of articulation%%%%
stopC(X) :-
 	X <> [consonant, manner=stop]:char.

fricC(X) :-
 	X <> [consonant, manner=fric]:char.

nasalC(X):-
 	X <> [consonant, manner=nasal]:char.


 %%%%%%% Combination of manner and place of articulation%%%%
dentfric(X):-        
 	X <> [place=dental, fricC]:char.

alvdent(X) :-
 	X <> [consonant, place=alveo(dental)]:char.

alvstop(X) :-
 	X <> [alvdent, stopC]:char.

alvfric(X) :-
 	X <> [alvdent, fricC]:char.

alvtrill(X) :-
 	X <> [alvdent, manner=trill]:char.

alvlateral(X) :-
 	X <> [alvdent, manner=lateral]:char.

alveither(X) :-
 	X <> [alvdent, manner=MANNER]:char,
	trigger(MANNER, (MANNER=trill; MANNER=lateral)).

postalv(X):-
 	X <> [place=alveo(post), fricC]:char.

uvufric(X):-
 	X <> [place=uvular, fricC]:char.

pharfric(X):-  
 	X <> [place=pharyngeal, fricC]:char.


 %%%%Special features%%%%

liquid(X) :-
	X <> alveither.

anyTanween(X) :-
 	tanween:char@X <-> *(_).


 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %%%%%%%%%% SOUNDS FEATURES%%%%%%%%%%%%

charDefaults(X) :-
	default(-tanween:char@X),
	default(-semivowel:char@X),
	default(-solar:char@X),
	default(-emph:char@X),
	default(-shadda:char@X),
	default(-long:char@X),
				%	default(-sukun:char@X),
	default(-dash:char@X),
	default(-phar:char@X).

sound(a, C) :-
	grapheme:char@C <-> a,
	sampa:char@C <-> a, 
	C <> [openV, unroundV, charDefaults]:char.

sound('a^', C) :-
	grapheme:char@C <-> a,
	sampa:char@C <->'a^', 
	C <> [openV, unroundV, +phar, charDefaults]:char.

sound(u, C) :-
	grapheme:char@C <-> u,
	sampa:char@C <->u, 
	C <> [closeV, roundV, charDefaults]:char.

sound('u^', C) :-
	grapheme:char@C <-> u,
	sampa:char@C <-> 'u^', 
	C <> [closeV, roundV, +phar, charDefaults]:char.


sound(i, C) :-
	grapheme:char@C <-> i,
	sampa:char@C <-> i, 
	C <> [closeV, unroundV, charDefaults]:char.

sound('i^', C) :-
	grapheme:char@C <-> i,
	sampa:char@C <-> 'i^', 
	C <> [closeV, unroundV, +phar, charDefaults]:char.


sound('A', C) :-
 	grapheme:char@C <-> 'A',
	sampa:char@C <-> aa,
  	C <>[+semivowel, +long, +open, -rounded, manner=glide, charDefaults ]:char.
	

sound('A^', C) :-
	grapheme:char@C <-> 'A',
	 sampa:char@C <-> 'aa^',
 	C <>[+semivowel, +long, +open, -rounded, +phar, manner=glide, charDefaults ]:char.

sound('Y', C) :-
	grapheme:char@C <-> 'Y',
 	sampa:char@C <-> aa,
 	C <> [longV, openV, unroundV, charDefaults]:char.

sound(y, C) :-
	grapheme:char@C <-> y,
 	sampa:char@C <-> S,
 	C <> [vowel=V, +semivowel, place=palatal, manner=glide, -open, -rounded, +long, charDefaults]:char,
	trigger(V, (V = - -> S = y; S = ii)).

sound('y^', C) :-
	grapheme:char@C <-> 'y^',
 	sampa:char@C <-> S,
 	C <> [vowel=V, +semivowel, place=palatal, manner= glide, -open, -rounded, +long, +phar, charDefaults]:char,
	trigger(V, (V = - -> S = y; S = 'ii^')).

sound(w, C) :-
	grapheme:char@C <-> w,
 	sampa:char@C <-> S,
 	C <> [vowel=V, +semivowel, place=labiovelar, manner= glide, -open, +rounded, +long, charDefaults]:char,
	trigger(V, (V = - -> S = w; S = uu)).

sound('w^', C) :-
	grapheme:char@C <-> 'w^',
 	sampa:char@C <-> S,
 	C <> [vowel=V, +semivowel, place=labiovelar, manner= glide, -open, +rounded, +long, +phar, charDefaults]:char,
	trigger(V, (V = - -> S = w; S = 'uu^')).

sound(b, C) :-
	grapheme:char@C <-> b,
	sampa:char@C <-> b, 
	C <>[place=bilabial, stopC,  charDefaults]:char.

sound(t, C) :-
	grapheme:char@C <-> t,
	sampa:char@C <-> t, 
	C <>[alvstop, +solar, charDefaults]:char.

sound(v, C) :-
	grapheme:char@C <-> v,
	sampa:char@C <-> v,
	C <>[dentfric, +solar, charDefaults]:char.

sound(j, C) :-
	grapheme:char@C <-> j,
	sampa:char@C <-> j, 
	C <>[postalv, charDefaults]:char.

sound('H', C) :-
	grapheme:char@C <-> 'H',
 	sampa:char@C <-> 'h^',
 	C <>[pharfric, charDefaults]:char.

sound(x, C) :-
	grapheme:char@C <-> x,
	sampa:char@C <-> x, 
 	C <> [uvufric, charDefaults]:char.

sound(d, C) :-
	grapheme:char@C <-> d,
	sampa:char@C <-> d, 
 	C <> [alvstop, +solar, charDefaults]:char.

sound(*, C) :-
	grapheme:char@C <-> '*',
 	sampa:char@C <-> 'D^',
 	C <> [dentfric, +solar, charDefaults]:char.

sound(r, C) :-
	grapheme:char@C <-> r,
	sampa:char@C <-> r, 
 	C <> [alvtrill, +solar, charDefaults]:char.

sound('r^', C) :-
 	grapheme:char@C <-> 'r^',
	sampa:char@C <-> 'r^', 
 	C <> [alvtrill, +phar, +solar, charDefaults] :char.

sound(z, C) :-
	grapheme:char@C <-> z,
	sampa:char@C <-> z, 
 	C <> [alvfric, +solar, charDefaults]:char.

sound(s, C) :-
	grapheme:char@C <-> s,
	sampa:char@C <-> s, 
 	C <> [alvfric, +solar, charDefaults]:char.

%% Use X instead of $ because HTK can't cope with $
sound('X', C) :-
	grapheme:char@C <-> 'X',
 	sampa:char@C <-> 'S^',
 	C <> [postalv, +solar, charDefaults]:char.

sound($, C) :-
	grapheme:char@C <-> '$',
 	sampa:char@C <-> 'S^',
 	C <> [postalv, +solar, charDefaults]:char.

sound('S', C) :-
	grapheme:char@C <-> 'S',
 	sampa:char@C <-> 's^',
 	C <> [alvfric, +emph, +solar, charDefaults]:char.

sound('D', C) :-
	grapheme:char@C <-> 'D',
 	sampa:char@C <-> 'd^',
 	C <> [alvstop, +emph, +solar, charDefaults]:char.

sound('T', C) :-
	grapheme:char@C <-> 'T',
 	sampa:char@C <-> 't^',
 	C <> [alvstop, +emph, +solar, charDefaults]:char.

sound('Z', C) :-
	grapheme:char@C <-> 'Z',
 	sampa:char@C <-> 'z^',
 	C <> [dentfric, +emph, +solar, charDefaults]:char.

sound('E', C) :-
	grapheme:char@C <-> 'E',
 	sampa:char@C <-> 'E',
 	C <>[pharfric, charDefaults]:char.

sound(g, C) :-
	grapheme:char@C <-> g,
	sampa:char@C <-> g, 
 	C <> [uvufric, charDefaults]:char.

sound(f, C) :-
	grapheme:char@C <-> f,
	sampa:char@C <-> f, 
 	C <> [place=labiodental, fricC, charDefaults]:char.

sound(q, C) :-
	grapheme:char@C <-> q,
	sampa:char@C <-> q, 
 	C <> [place=uvular, stopC, charDefaults]:char.

sound(k, C) :-
	grapheme:char@C <-> k,
	sampa:char@C <-> k, 
 	C <> [place=velar, stopC, charDefaults] :char.

sound(l, C) :-
	grapheme:char@C <-> l,
	sampa:char@C <-> l, 
 	C <> [alvlateral, +solar, charDefaults] :char.

sound('l^', C) :-
  	grapheme:char@C <-> 'l^',
	sampa:char@C <-> 'l^', 
 	C <> [alvlateral, +solar, +phar, charDefaults] :char.

sound(m, C) :-
	grapheme:char@C <-> m,
	sampa:char@C <-> m, 
 	C <> [place=bilabial, nasalC, charDefaults]:char.


sound(n, C) :-
	grapheme:char@C <-> n,
	sampa:char@C <-> n, 
 	C <> [alvdent, nasalC, +solar, charDefaults]:char.

sound('M' , C) :-
	grapheme:char@C <-> 'M',
	sampa:char@C <-> 'M', 
 	C <> [place=nasallabiodental, nasalC, +solar, charDefaults]:char.

sound(e , C) :-
	grapheme:char@C <-> e,
	sampa:char@C <-> e, 
 	C <> [place=nasalpalatal, nasalC, +solar, charDefaults]:char.

sound(c, C) :-
 	grapheme:char@C <-> c,
 	sampa:char@C <-> c, 
  	C <> [place=nasalvelar, nasalC, +solar, charDefaults]:char.

sound(h, C) :-
	grapheme:char@C <-> h,
	sampa:char@C <-> h, 
 	C <> [place=glottal, fricC, charDefaults]:char.

sound('Q', C) :-
 	grapheme:char@C <-> 'Q',       
 	sampa:char@C <-> 'Q',
	C <> [place=glottal, stopC, charDefaults]:char.

sound('\'', C) :-
 	grapheme:char@C <-> '\'',        %('Q';'>';'&';'<';'}'), 
 	sampa:char@C <-> 'Q',
	C <> [place=glottal, stopC, charDefaults]:char.

sound('>', C) :-
 	grapheme:char@C <->'>',    
 	sampa:char@C <-> 'Q',
	C <> [place=glottal, stopC, charDefaults]:char.

sound('&', C) :-
 	grapheme:char@C <->'&',      
 	sampa:char@C <-> 'Q',
	C <> [place=glottal, stopC, charDefaults]:char.
sound('W', C) :-
 	grapheme:char@C <->'W',      
 	sampa:char@C <-> 'Q',
	C <> [place=glottal, stopC, charDefaults]:char.
sound('<', C) :-
 	grapheme:char@C <->'<',      
 	sampa:char@C <-> 'Q',
	C <> [place=glottal, stopC, charDefaults]:char.

sound('}', C) :-
 	grapheme:char@C <->'}',     
 	sampa:char@C <-> 'Q',
	C <> [place=glottal, stopC, charDefaults]:char.

sound('|', C) :-
  	grapheme:char@C <->'|',      
  	sampa:char@C <-> 'Q',
 	C <> [place=glottal, stopC, charDefaults]:char.

% sound('|', C) :-
%   	grapheme:char@C <-> '|',
%    	sampa:char@C <-> 'Qaa',
%    	C <> [place=glottal, manner=stop, charDefaults]:char.


sound('`', C) :-
 	grapheme:char@C <->'`',      
 	sampa:char@C <-> 'aa',
	C <>[longV, +open, -rounded, charDefaults ]:char.

sound('`^', C) :-
 	grapheme:char@C <->'`^',      
 	sampa:char@C <-> 'aa^',
	C <>[longV, +open, -rounded, +phar, charDefaults ]:char.

sound('F', C) :-
	grapheme:char@C <-> 'F',
% 	sampa:char@C <-> 'an',
	tanween:char@C <-> *fatH.
%	C <> [+vowel, charDefaults]:char.

% sound('F', C) :-
% 	grapheme:char@C <-> 'F',
%  	sampa:char@C <-> 'a^n',
% 	tanween:char@C <-> *fatH,
% 	C <> [+vowel, +phar, charDefaults]:char.

sound('K', C) :-
	grapheme:char@C <-> 'K',
 %	sampa:char@C <-> 'in',
 	tanween:char@C <-> *kasr.
%	C <> [+vowel, charDefaults]:char.

% sound('K', C) :-
% 	grapheme:char@C <-> 'K',
%  	sampa:char@C <-> 'i^n',
%  	tanween:char@C <-> *kasr,
% 	C <> [+vowel, +phar, charDefaults]:char.


sound('N', C) :-
	grapheme:char@C <-> 'N',
 %	sampa:char@C <-> 'un',
 	tanween:char@C <-> *dam.
%	C <> [+vowel,  charDefaults]:char.

% sound('N', C) :-
% 	grapheme:char@C <-> 'N',
%  	sampa:char@C <-> 'u^n',
%  	tanween:char@C <-> *dam,
% 	C <> [+vowel, +phar, charDefaults]:char.

sound(o, C) :-
	grapheme:char@C <-> o,
 %	sampa:char@C <-> ' ',
 	C <>[+sukun, +vowel, charDefaults]:char.


sound(p, C):-
	grapheme:char@C <-> p,
	C <>[consonant, charDefaults]:char.

sound('~', C):-
	grapheme:char@C <-> '~',
	C <>[+shadda, -vowel, -open,  charDefaults]:char.

sound(' ', C):-
 	grapheme:char@C <-> ' ',
	sampa:char@C <-> ' ',
 	C <>[-vowel, charDefaults]:char.
	
sound('-', C):-
 	grapheme:char@C <-> '-',
	sampa:char@C <-> '-',
 	C <>[+dash, -vowel, charDefaults]:char.
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% graph2sampa('A', aa):-        	
%  	!.
% graph2sampa('H','h^'):-        	%%% I need to set a different symbol for letters with ^ %%% 
%  	!.
% graph2sampa(*,'D^'):-
%  	!.
% graph2sampa('$','S^'):-
%  	!.
% graph2sampa('S','s^'):-
%  	!.
% graph2sampa('D','d^'):-
%  	!.
% graph2sampa('T','t^'):-
%  	!.
% graph2sampa('Z','z^'):-
%  	!.
% graph2sampa('E','E^'):-
%  	!.
% graph2sampa('Q','Q'):-		%;'>';'&';'<';'}'%    NR
%  	!.
% graph2sampa('Y',aa):-
%  	!.
% graph2sampa('|','Qaa'):-
%  	!.
% graph2sampa('F',an):-
%  	!.
% graph2sampa('N',un):-
%  	!.
% graph2sampa('K',in):-
%  	!.
% graph2sampa(o,''):-
%  	!.
% graph2sampa(X,X).

% convertGraph([],[]).
% convertGraph([H1|T1],[H2|T2]):-
%  	graph2sampa(H1,H2),
%  	convertGraph(T1,T2).