:- multifile sig/2.


 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %%%%%%%%%%Words with irrigular spellings

testRule(S0, S1, S3, RE) :-     %%%no stress  also uncomment  listOfAtomsToString   
   	string2chars(S0, S1),
   	preprocess(S1, S2),
  	applyImanRule(S2, S3),
 	resplitWords(S3, RWORDS),
	%% pretty(RWORDS),
 	assignStressToWords(RWORDS),
  	findall([sampa:char@X, final:pos@word:char@X ], (member(X, S3), (sound(SOUND, X) -> true; fail)), SN),  % 
	list_string(SN, RE).


% testRule(S0, S1, S3, RE) :-     %%% to add the stress value   
%    	string2chars(S0, S1),
%    	preprocess(S1, S2),
%   	applyImanRule(S2, S3),
%  	resplitWords(S3, RWORDS),
% 	%% pretty(RWORDS),
%  	assignStressToWords(RWORDS),
%   	findall([sampa:char@X,  stress:syll@syllable:char@X, final:pos@word:char@X ], (member(X, S3), (sound(SOUND, X) -> true; fail)), SN),  % 
% 	list_string(SN, RE).

string2chars([H | T], CHARSN) :-
	atom(H),
	!,
	rcharacters([H | T], CHARSN).
string2chars(STRING, CHARS) :-
	findall(A, (member(C, STRING), atom_chars(A, [C])), ATOMS),
	string2chars(ATOMS, CHARS).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% listOfAtomsToString([], []).
% listOfAtomsToString([H0 | T0], C) :-
% 	atom_codes(H0, H1),
% 	append("/", H1,R),
% 	append(R, T1, C),
% 	listOfAtomsToString(T0, T1).

% list_string(List, String) :-
% 	listOfAtomsToString(List, Codes),
% 	atom_codes(String, Codes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% listOfAtomsToString([], []).
% listOfAtomsToString([[H0, S0] | T0], C) :-
% 	atom_codes(H0, H1),
% 	atom_codes(S0, S1),
% 	listOfAtomsToString(T0, T1),
% 	appendAll(["/", H1, S1, "/ ", T1], C).

% list_string(List, String) :-
% 	listOfAtomsToString(List, Codes),
% 	atom_codes(String, Codes).



/***	
listOfAtomsToString([], []).
listOfAtomsToString([[H0, S0, F] | T0], C) :-
  	atom_codes(H0, H1),
  	atom_codes(S0, S1),
  	(F= + ->
  	 append("##", [], F1);
  	 true),
  	listOfAtomsToString(T0, T1),
  	appendAll([H1, S1, " ", F1,  T1], C).
***/

listOfAtomsToString([], []). %%% no stress
listOfAtomsToString([[H0,  F] | T0], C) :-
    	atom_codes(H0, H1),
    	(F= + ->
    	 append("##", [], F1);
    	 true),
    	listOfAtomsToString(T0, T1),
    	appendAll([H1, " ", F1,  T1], C).



list_string(List, String) :-
  	listOfAtomsToString(List, Codes),
  	atom_codes(String, Codes).





%%%%%%%%Pre-Process (fixsv, remsukuns, fixphar, fixboundaries)%%%%%%%%%%%%

preprocess(TXT0, TXTN) :-
	fixTanween(TXT0, TXT1),
	fixFem(TXT1, TXT2),
	remshadda(TXT2, TXT3),
	remsukuns(TXT3, TXT4),
	fixboundaries(TXT4, TXT5),
	fixHamza(TXT5,TXTN),
	fixsv(TXTN).
%	!,
%	fixph(TXT6 , TXTN).





invsound(X, Y) :-
	sound(Y, X).


fixTanween([], []).         %%% For tanween and adding and removing 'a'from Alif maqsura and adding 'a' to the difiinite article and fixing feminine marker.


fixTanween([C1 | T0], [V1, C2 | T1]) :-
	C1 <> invsound('F'),
	V1 <> invsound(a),
	C2 <> invsound(n),
  	!,
  	fixTanween(T0, T1).

fixTanween([A, C1 | T0], [V1, C2 | T1]) :-
	A <> invsound('A'),
	C1 <> invsound('F'),
	V1 <> invsound(a),
	C2 <> invsound(n),
  	!,
  	fixTanween(T0, T1).

fixTanween([Y, C1 | T0], [V1, C2 | T1]) :-
	Y <> invsound('Y'),
	C1 <> invsound('F'),
	V1 <> invsound(a),
	C2 <> invsound(n),
  	!,
  	fixTanween(T0, T1).

fixTanween([C1 | T0], [V1, C2 | T1]) :-
	C1 <> invsound('K'),
	V1 <> invsound(i),
	C2 <> invsound(n),
  	!,
  	fixTanween(T0, T1).

fixTanween([C1 | T0], [V1, C2 | T1]) :-
	C1 <> invsound('N'),
	V1 <> invsound(u),
	C2 <> invsound(n),
  	!,
  	fixTanween(T0, T1).


fixTanween([A, Y | T0], [Y | T1]) :-                    %%% For Alif maqsuura
	A <> invsound(a),
	Y <> invsound('Y'),
  	!,
  	fixTanween(T0, T1).


fixTanween([X, A, L, C | T0], [X, A, Aa, L, C | T1]) :- %% We need to apply this rule before removing the spaces This rule is devided into to steps one in (fixTamween) and the other in (fixHmaza)
    	(X <> shortV; sound(' ', X)),
    	A <> invsound('A'),
    	L <> invsound(l),
    	Aa <> invsound(a),
    	(C <> [+semivowel]:char; C <> [-vowel]:char),
      	!,
      	fixTanween(T0, T1).
fixTanween([H| T0], [H| T1]) :-
 	fixTanween(T0, T1).



%%%%%%%%Feminine marker
fixFem([], []).

fixFem([P,V|T0], [T,V| T1]) :-
	P <> invsound(p),
     	T <> invsound(t),
     	V <> shortV,
%	+final:pos@word:char@V,
%	word:char@P <-> word:char@T,
     	!,
 	fixFem(T0, T1).

fixFem([P|T0], T1):-
   	P <> invsound(p),
    %  	+final:pos@word:char@P,
	!,
    	fixFem(T0, T1).

fixFem([H| T0], [H| T1]) :-
 	fixFem(T0, T1).


remshadda([], []).

remshadda([A, L, SO, S | T0], [A, L, SO | T1]):- %%% removing shadda after the solar letter in the difinite article--@ the beginning of utterance
 	A <> invsound('A'),
 	L <> invsound(l),			
 	+solar:char@SO,
 	S<> invsound('~'),
    	!,
    	remshadda(T0, T1).

remshadda([A, Aa, L, SO, S | T0], [A, Aa, L, SO | T1]):- %%% removing shadda after the solar letter in the difinite article--@before prepositional or space
 	A <> invsound('A'),
 	Aa<> invsound(a),
 	L <> invsound(l),			
 	+solar:char@SO,
 	S<> invsound('~'),
    	!,
    	remshadda(T0, T1).

remshadda([L, I, L2, SO, S | T0], [L, I, L2, SO | T1]):-
	L <> invsound(l),
	I<> invsound(i),
	L <> invsound(l),			
	+solar:char@SO,
	S<> invsound('~'),
   	!,
   	remshadda(T0, T1).



remshadda([X0, S | T0], [X0,X2 | T1]):-                
  	 -vowel:char@X0,
	S <> invsound('~'),
	X0\(syll:char) <-> X1\(syll:char),
	X1\(word:char) <-> X2\(word:char),
   	!,
   	remshadda(T0, T1).


remshadda([H | T0], [H | T1]) :-
 	remshadda(T0, T1).



remsukuns([], []).                                 

remsukuns([W, S, A|T0],[W, S, A|T1]):-          %%% to avoid removing sukun from words like (daEawoA)
	W <> invsound(w),
	S <> invsound(o),
	A <> invsound('A'),
	!,
	remsukuns(T0, T1).

remsukuns([S | L0], [S | L1]):-
     	-sukun:char@S,
     	!,
     	remsukuns(L0, L1).
remsukuns([_ | L0], L1) :-
     	remsukuns(L0, L1).



fixHamza([], []).       %%% For mad Alif, Extra Alid after group wAw, glides, and adding 'a' to the difinite article at the beginning of the utterance


fixHamza([C1 | T0], [C1, V | T1]) :- %%% For mad Alif
 	C1 <> invsound('|'),
 	V <> invsound('A'),
   	!,
   	fixHamza(T0, T1).

fixHamza([U, W, A| T0], [LW | T1]) :-  %%for extra 'A' after group waw
 	U <> invsound(u),
 	W <> [grapheme=w, +long, -open, +rounded ]:char,
 	A <> invsound('A'),
 	+final:pos@word:char@A,
 	!,
 	LW <> [grapheme=w, +long, +vowel, -open, +rounded]:char,
	word:char@A <-> word:char@LW,
   	fixHamza(T0, T1).


fixHamza([SA, W, S, A| T0], [SA, W1 | T1]) :-          %%for extra 'A' after group waw in cases like 'daEawoA'
	SA <> invsound(a),
	W <> invsound(w),
	S <> invsound(o),
 	A <> invsound('A'),
 	+final:pos@word:char@A,
	!,
	W1 <> invsound(w),
	word:char@A <-> word:char@W1,

   	fixHamza(T0, T1).

fixHamza([SH, G| T0], [LV | T1]) :-
    	SH <> [shortV, -open, rounded=R ]:char,
    	G <> [grapheme= Y, +semivowel, -open, rounded=R]:char,
    	+final:pos@word:char@G,
    	LV <> [grapheme= Y, +long, +vowel, -open, rounded=R]:char,
  	+final:pos@word:char@LV,
      	!,
      	fixHamza(T0, T1).

fixHamza([SH, G, C| T0], [LV, C | T1]) :- %%% For long vowels
     	SH <> [shortV, -open, rounded=R ]:char,
     	G <> [grapheme= Y, +semivowel, -open, rounded=R]:char,
     	LV <> [grapheme= Y, +long, +vowel, -open, rounded=R]:char,
	word:char@G <-> word:char@LV,
     	C <> [-vowel, -shadda, -open]:char,
	\+ C = G,
       	!,
       	fixHamza(T0, T1).

fixHamza([A, L, C | T0], [A, Aa, L, C | T1]) :-	%%% To allow the words at the beginning of the utterance%%% %%This is the other part of the rule in fixTanween%%
   	A <> invsound('A'),
  	+initial:pos@utt:char@A,
   	L <> invsound(l),
   	(C <> [-vowel]:char;
	 C <> [+semivowel]:char),
   	Aa <> invsound(a),
	-final:pos@word:char@Aa,
     	!,
     	fixHamza(T0, T1).


fixHamza([H| T0], [H| T1]) :-
 	fixHamza(T0, T1).




fixboundaries(TXT0, TXT1) :-
  	fixuboundaries(TXT0, +),
	fixwboundaries(TXT0, TXT1).


fixuboundaries([],  _).
fixuboundaries([H | T0], I) :-
	I <-> initial:pos@utt:char@H,
  	(I = + -> initial:pos@word:char@H = +; true),
   	fixuboundaries(T0, -).

fixwboundaries([],  []).
fixwboundaries([Y, X | T0], T1):-
  	+initial:pos@word:char@X,
	grapheme:char@Y <-> ' ',
	!,
  	fixwboundaries([X | T0], T1).
fixwboundaries([X, Y | T0], [X |T1]):-
	grapheme:char@Y <-> ' ',
   	+final:pos@word:char@X,
  	default(-initial:pos@word:char@X),
 	!,
   	fixwboundaries([Y | T0], T1).
fixwboundaries([H | T0], [H | T1]) :-
  	default(-initial:pos@word:char@H),
	(T0 = [] ->
	 +final:pos@word:char@H;
	 default(-final:pos@word:char@H)),
	fixwboundaries(T0, T1).


fixsv([_]).

fixsv([S | T]) :-
    	+final:pos@word:char@S,
    	!,
    	fixsv(T).

fixsv([C0, A, C1 | L0]) :-
       	C0 <> consonant,
	A <> invsound('A'),
	A <> vowel,
       	C1 <> consonant,
     	fixsv([C1 | L0]).

fixsv([C0, C1, V | L0]) :-
       	C0 <> consonant,
       	C1 <> consonant,
       	V <> vowel,
       	fixsv([C1, V | L0]).

fixsv([C, V | L0]) :-
   	C <> consonant,
   	V <> vowel,
   	!,
   	fixsv([V | L0]).


fixsv([V, C| L0]) :-
   	C <> consonant,
   	V <> vowel,
   	!,
   	fixsv([C | L0]).

fixsv([C | L0]) :-
   	C <> consonant,
   	fixsv(L0).


	    
fixph([], []).

 % %%%%%%%%Pharyngealisation of the 's' sound and the vowel follows it when preceed a 'T' sound.
fixph([S, V0, T | T0],[S1, V1, T|  T1]):- %% CVC
   	S <> invsound(s),
   	T <> invsound('T'),
   	S1 <> invsound('S'),
    	!,
    	V0 <> [vowel, open=O, rounded=R, long=L]:char,
        	V1 <> [vowel, open=O, rounded=R, long=L, +phar]:char,
   	word:char@S <-> word:char@S1,
 	-final:pos@word:char@V0,
   	word:char@V0 <-> word:char@V1,	
 	fixph([T|T0], [T|T1]).

fixph([S, T |T0],[S1, T| T1]):-	%%CC
  	S <> invsound(s),
  	T <> invsound('T'),
  	S1 <> invsound('S'),	
  	word:char@S <-> word:char@S1,
   	!,
   	fixph([T|T0], [T|T1]).


fixph([S, V0, C, V,  T | T0],[S1, V1, C, V, T|  T1]):- %%CVCVC
  	S <> invsound(s),
  	T <> invsound('T'),
  	S1 <> invsound('S'),		
   	grapheme:char@C <-> G,
   	\+ G = t,
    	+vowel:char@V,
  	word:char@S <-> word:char@S1,
  	word:char@V0 <-> word:char@V1,	
    	!,
    	V0 <> [vowel, open=O, rounded=R, long=L]:char,
        	V1 <> [vowel, open=O, rounded=R, long=L, +phar]:char,
 	fixph([T|T0], [T|T1]).

fixph([S, V0, C,  T | T0],[S1, V1, C, T|  T1]):- %%CVCC
  	S <> invsound(s),
  	T <> invsound('T'),
  	S1 <> invsound('S'),
   	grapheme:char@C <-> G,
  	word:char@S <-> word:char@S1,
  	word:char@V0 <-> word:char@V1,	
   	\+ G = t,		
    	!,
    	V0 <> [vowel, open=O, rounded=R, long=L]:char,
        	V1 <> [vowel, open=O, rounded=R, long=L, +phar]:char,
 	fixph([T|T0], [T|T1]).


fixph([S, C, V, T | T0],[S1, C, V, T|  T1]):- %%CCVC
  	S <> invsound(s),
  	T <> invsound('T'),
  	S1 <> invsound('S'),
  	grapheme:char@C <-> G,
   	\+ G = t,
    	+vowel:char@V,
  	word:char@S <-> word:char@S1,
    	!,
 	fixph([T|T0], [T|T1]).

   %%%%%Pharyngealisation of the vowels sorrounded the emphatic and "q"  sounds

fixph([V0, E |T0],[V1, E| T1]):-        %% @ the end of the word
       	+emph:char@E,
 	+final:pos@word:char@E,
      	word:char@V0 <-> word:char@V1,	
   	!,
   	V0 <> [vowel, open=O, rounded=R, long=L]:char,
   	V1 <> [vowel, open=O, rounded=R, long=L, +phar]:char,
   	fixph(T0, T1).


fixph([V0, E, C |T0],[V1, E, C| T1]):-        %% @ the end of the syllable
 	C <> [consonant],
       	+emph:char@E,
      	word:char@V0 <-> word:char@V1,	
   	!,
   	V0 <> [vowel, open=O, rounded=R, long=L]:char,
   	V1 <> [vowel, open=O, rounded=R, long=L, +phar]:char,
   	fixph([C|T0], [C|T1]).

fixph([E, V0 |T0],[E, V1| T1]):-           %%% @ the beginning of the syllable
 	+emph:char@E,
    	word:char@V0 <-> word:char@V1,
 	!,
     	V0 <> [vowel, open=O, rounded=R, long=L]:char,
     	V1 <> [vowel, open=O, rounded=R, long=L, +phar]:char,
     	fixph(T0, T1).

fixph([V0, Q |T0],[V1, Q| T1]):- %% @ the end of the word
  	(Q <>[place=uvular, stopC]:char;
  	 Q <> [uvufric]:char),
  	+final:pos@word:char@Q, 
       	word:char@V0 <-> word:char@V1,
    	!,
    	V0 <> [openV, long=L]:char,
    	V1 <> [openV,  long=L, +phar]:char,
    	fixph(T0, T1).

fixph([V0, Q, C |T0],[V1, Q, C| T1]):- %%% @ the end of the syllable
  	C <> [consonant],
  	(Q <>[place=uvular, stopC]:char;
  	 Q <> [uvufric]:char),
       	word:char@V0 <-> word:char@V1,
    	!,
    	V0 <> [openV, long=L]:char,
    	V1 <> [openV,  long=L, +phar]:char,
    	fixph([C|T0], [C|T1]).

 fixph([Q, V0 |T0],[Q, V1| T1]):- %%% @ the beginning of the syllable
  	(Q <> [place=uvular, stopC]:char;
  	 Q <> [uvufric]:char),
     	word:char@V0 <-> word:char@V1,
  	!,
      	V0 <> [openV, long=L]:char,
      	V1 <> [openV, long=L, +phar]:char,
     	fixph(T0, T1).

 %%%%% Pharyngealisation of the 'r' sound with the adjacent vowel %%%%%

fixph([V0,R0 |T0], [V1, R1| T1]):-                %%% @ the end of the word
 	V0 <> [vowel, open=O, rounded=R, long=L]:char,
 	R0 <> [grapheme= r, sampa= r, alvtrill, +solar, charDefaults]:char,
 	+final:pos@word:char@R0,
 	V1 <> [vowel, open=O, rounded=R, long=L, +phar]:char,
 	R1 <> [grapheme= 'r^', sampa= 'r^', alvtrill, +solar, +phar, charDefaults]:char,
 	\+ (O = -, R = -),
  	word:char@V0 <-> word:char@V1,
  	word:char@R0 <-> word:char@R1,
        	!,
 	fixph(T0, T1).

fixph([V0,R0, C |T0], [V1, R1, C1| T1]):-      %%% @ the end of the syllable
 	C <> [-vowel]:char,
 	V0 <> [vowel, open=O, rounded=R, long=L]:char,
 	R0 <> [grapheme= r, sampa= r, alvtrill, +solar, charDefaults]:char,
 	V1 <> [vowel, open=O, rounded=R, long=L, +phar]:char,
 	R1 <> [grapheme= 'r^', sampa= 'r^', alvtrill, +solar, +phar, charDefaults]:char,
 	\+ (O = -, R = -),
  	word:char@V0 <-> word:char@V1,
  	word:char@R0 <-> word:char@R1,
        	!,
 	fixph([C|T0], [C1|T1]).

fixph([R0, V0 |T0],[R1, V1| T1]):-              %%@ the beginning of the syllable
  	R0 <> [grapheme= r, sampa= r, alvtrill, +solar, charDefaults]:char,
  	V0 <> [vowel, open=O, rounded=R, long=L]:char,
    	R1 <> [grapheme= 'r^', sampa= 'r^', alvtrill, +solar, +phar, charDefaults]:char,
    	V1 <> [vowel, open=O, rounded=R, long=L, +phar]:char,
    	\+ (O = -, R = -),
  	word:char@V0 <-> word:char@V1,
  	word:char@R0 <-> word:char@R1,
  	!,
     	fixph([V1|T0] , [V1|T1]).


 %%%%% Pharyngealisation of the 'l' sound with the adjacent vowel in "Allah" %%%%%

% fixph([V0, A, V1,  L0, L1, M0, H| T0], [V0, A, V1, L2, L3, M1, H| T1]):-
% 	V0 <> [vowel, open=O, rounded=R]:char,
% 	\+ (O = -, R = -),
%  	A <> invsound('A'),
%  	V1 <> invsound(a),
%  	L0 <> invsound(l),
%  	L1 <> invsound(l),
%  	L2 <> invsound('l^'),
%  	L3 <> invsound('l^'),
%  	M0 <> invsound('`'),
%  	M1 <> invsound('`^'),
%    	H <> invsound(h),
%  	word:char@L0 <-> word:char@L2,
%  	word:char@L1 <-> word:char@L3,
%  	word:char@M0 <-> word:char@M1,
% 	!,
%  	fixph(T0, T1).


% fixph([A, V1,  L0, L1, M0, H| T0], [A, V1, L2, L3, M1, H| T1]):-
%   	A <> invsound('A'),
%    	+initial:pos@utt:char@A,
%   	V1 <> invsound(a),
%   	L0 <> invsound(l),
%   	L1 <> invsound(l),
%   	L2 <> invsound('l^'),
%   	L3 <> invsound('l^'),
%   	M0 <> invsound('`'),
%   	M1 <> invsound('`^'),
%     	H <> invsound(h),
%   	word:char@L0 <-> word:char@L2,
%   	word:char@L1 <-> word:char@L3,
%   	word:char@M0 <-> word:char@M1,
% 	!,
% 	fixph(T0, T1).



fixph([H | T0], [H | T1]) :-
    	fixph(T0, T1).


justGraphemes(L0, L1) :-
	findall((sampa:char@X=grapheme:char@X), member(X, L0), L1).

%%%%%%% Apply Iman Rule %%%%%%%%%%%%%
applyImanRule(_LEFT, [], []).
applyImanRule(LEFT, SURFACE, UNDERLYINGN) :-
	imanRule(LEFT, SURFACE, UNDERLYING0),
	(flag(showImanRules) ->
	 (justGraphemes(LEFT, LEFTG),
	  justGraphemes(SURFACE, SURFACEG),
	  justGraphemes(UNDERLYING0, UNDERLYING0G),
	  pretty(imanRule(LEFTG, SURFACEG, UNDERLYING0G)));
	 true),
	applyImanRule(LEFT, UNDERLYING0, UNDERLYINGN).
applyImanRule(LEFT, [H | S], [H | U]) :-
	applyImanRule([H | LEFT], S, U).

applyImanRule(S, U) :-
	applyImanRule([], S, U).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% GLIDES RULES %%%%%%%%%%


     [{v0, artic=A},'A' , a, {alvlateral}, 'A', {shortV, -open, -rounded, word=WW}, {c0, grapheme=G, word=WWW}] %%% IF  'AL' WAS PROCEEDED BY A VOWEL %%%%% 
     ==>
     [{v1, artic=A, shortV, word=W},{ grapheme='A',word=W1},  {alvlateral,word=W2},  {'A',word=W3} , {shortV, -open, -rounded, word=WW}, {c1, grapheme=G, word=WWW} ] :
     [???] ## [ ???]:-
     	 +final:pos@W, -initial:pos@W,
     	 -final:pos@W1, +initial:pos@W1,
  	 -final:pos@W2, -initial:pos@W2,
          -final:pos@W3, -initial:pos@W3.




    [{v0,  artic=A},{'A', word=WWW} , {grapheme=a,word=WWW1},  {alvlateral}, {c1, grapheme=G, artic=AA,  +solar}]  %%% modified word boundary           
    ==>
    [{v1, artic=A, shortV, word=WW}, {'A', word=WWW}, {alvlateral,word=W}, {c2, grapheme=G, artic=AA,  +solar, word=W1}] :
    [???] ## [???]:-
    	 +initial:pos@WWW,
    	 +final:pos@WW,
    	 -final:pos@W, -initial:pos@W,
    	  -final:pos@WWW1,
    	 -final:pos@W1.

  %  				% +initial:pos@W.



     [{v0, artic=A }, {'A', word=WWW}, a, {alvlateral},  {c0, grapheme=G, -solar}]    %%%modofied               
     ==>
     [{v1, artic=A, shortV,  word=WW},  {'A', word=WWW}, {alvlateral, word=W}, {c1, grapheme=G,  -solar, word=W1}] :
     [???] ## [???]:-
     	  +initial:pos@WWW,
     	  + final:pos@WW,
     	  - final:pos@W,-initial:pos@W,
     	  -final:pos@W1.


    [{v0, shortV}, {'A', word=WWW}, a, {alvlateral},  {c0, grapheme=G, -solar}]         %%% when preceeded bu prepositional             
    ==>
    [{v0, shortV, word=WW}, {alvlateral, word=WW}, {c1, grapheme=G,  -solar, word=WW}] :
    [???] ## [???]:-
    	  -initial:pos@WWW,
    	 - final:pos@WW.




   [{c0, grapheme=G}, {'A',word=W},a, {alvlateral}, 'A']           %%%%% IF  'AL' WAS PROCEEDED BY A CONSONANT %%%%%                 
   ==>
   [{c1,grapheme=G, word=WW},  {alvlateral, word=W2}]:
   [???] ## [{shortV, -open, -rounded}, ???]:-
   	+initial:pos@W,
   	+final:pos@WW, -initial:pos@WW,
   	-final:pos@W2, +initial:pos@W2.



    [{c4, grapheme=GG}, {'A',word=W}, {grapheme=a,word=WWW1}, l , {c0, grapheme=G, artic=AA, +solar}]    %%modified               
    ==>
    [{c5, grapheme=GG, word=WW}, {alvlateral, word=W1 }, {c2, grapheme=G, artic=AA, +solar, word=W2}]:
    [???] ## [ ???]:-
     	+final:pos@WW,
     	+initial:pos@W,
     	- final:pos@W1, +initial:pos@W1,
       	-final:pos@W2,
    	-final:pos@WWW1,
    	-final:pos@W2.



  [{c3, grapheme=GG},{'A',word=W}, a,  {alvlateral}, {c0, grapheme=G, -solar}]  %modified
  ==>
  [{c4, grapheme=GG, word=W4}, {alvlateral, word=W1}, {c1, grapheme=G,  -solar, word=W2}]: 
  [???] ## [???]:-
  	+initial:pos@W,
  	-final:pos@W1, +initial:pos@W1,
  	-final:pos@W2,
  	+final:pos@W4.


   [{'A', utt=U, word=W} ,  {v0, +open, -long}, {alvlateral, word=WW}, 'A'] %%%% IF 'AL'WAS AT THE BEGINNING OF THE UTTERANCE%%%%                         
   ==>
   [{'\'', word=W}, {v0, +open, -long},  {alvlateral, word=WW}] :
   [???] ## [{shortV, -open, -rounded}, ???]:-            
          	+initial:pos@U.
	


   [{'A', utt=U, word=W} , {v0, +open, -long},  {alvlateral, word=WW}]                         
   ==>
   [{'\'', word=W}, {v0, +open, -long}, {c0, grapheme=G, +solar, word=WW}] :
   [???] ## [{c1, grapheme=G, +solar}, ???]:-            
   	+initial:pos@U.


   [{'A', utt=U, word=W} , {v0, +open, -long}, {alvlateral, word=WW}]                
   ==>
   [{'\'', word=W} , {v0, +open, -long}, {alvlateral, word=WW}]:
   [???] ## [{c0, -solar}, ???]:-                              
         	+initial:pos@U.


     %%%%%%%% HAMZAT ALWASL RULES%%%%%%%%

   [{v0, shortV},{'A', word= WW}, {v1, closeV}, {c0, grapheme=G, artic=B}, {c1, grapheme=G1, artic=B1}] %%% IF HAMZAT ALWASL  WAS PROCEEDED BY A PREPOSITIONAL %%%%%   %%% OK%%%
    ==>
    [{v0, shortV, word=W2}, {c2, grapheme=G, artic=B, word=W2}, {c3, grapheme=G1, artic=B1, word=W2}] :
    [???] ## [ v3, ???]:-
   	  -initial:pos@WW,
    	 -final:pos@W2.
 

     [{v0,  artic=A}, {'A', word=WW}, {v1, closeV,  artic=C}, {c0, grapheme=G, artic=B}, {c1, grapheme=G1, artic=B1}] %%% IF HAMZAT ALWASL  WAS PROCEEDED BY A VOWEL %%%%%   %%% OK%%% modified
     ==>
     [{v2,  shortV, artic=A, word=W2}, {'A', word=W5},  {v3, closeV, artic=C,  word=X},  {c2, grapheme=G, artic=B, word=W}, {c3, grapheme=G1, artic=B1, word=W1}] :
     [???] ## [ v3, ???]:-
     	  +initial:pos@WW,
     	  +initial:pos@W5,
     	  -final:pos@W, -initial:pos@W,
     	  -final:pos@X, -initial:pos@X,
     	  -final:pos@W1,
     	  +final:pos@W2.


    [{c4, grapheme=G4},{'A',word=W}, {v0, closeV}, {c0, grapheme=G, artic=A}, {c1, grapheme=G1, artic=A1}] %%% IF HAMZAT ALWASL  WAS PROCEEDED BY A CONSONANT %%%%%    %%% OK%%
    ==>
    [{c5, grapheme=G4, word=W4}, {c2, grapheme=G, artic=A, word=W1}, {c3, grapheme=G1, artic=A1, word=W2}] :
    [???] ## [ v1, ???] :-
    	+initial:pos@W,
   	+final:pos@W4, -initial:pos@W4,
   	-final:pos@W1, +initial:pos@W1,
   	-final:pos@W2, -initial:pos@W2. 




   [{'A', utt=U, word=WW}, {v0, closeV}]	% AT THE BEGINNING OF THE UTTERANCE%%
   ==>
   [{'\'', word=WW}, {v0, closeV}] :
   [] ## [c0, c1, v1, ???]:-
        	+initial:pos@U.

 

     %%%%%%%%%%PREPOSITIONAL 'L' RULE WHEN ATTACHED TO +SOLAR DEFINITE ARTICLE %%%%%%%%%%%

   ['A'] %%GOOD      
   ==>
   []:
   [???, l, i, l] ## [{shortV, closeV, unroundV}, c0,???].


   [{l}] %%% OK %%%                  
   ==>
   [{c0, grapheme=G, word=WW}]:		% W@pos@word:char
   [???,{l, word=W}, i] ## [{c1, grapheme=G, +solar},???]:-
   	 +initial:pos@W,
   	 -final:pos@WW.

     %%%%%%%%%%% ASSIMILATION RULES %%%%%%%%%%%%        

     [{alvdent, nasalC, grapheme=G0, word=W}]
     ==>
     [{c0, grapheme=G1, artic=ARTIC, word=W}] :                 
     [???] ## [{c1, artic=ARTIC, grapheme=G1}:C1, ???]:-
      	\+ G0 =  G1,
        	(nasalC(C1); liquid(C1)).



    [{n, word=W}] 
    ==>
    [{m, word=W}] :
    [???] ## [b, ???].


    [{n, word=W}] 
    ==>
    [{'M',word=W}] :                           
    [???] ## [f, ???].


    [{n, word=W}]                     
    ==>
    [{c, word=W}] :

    [???] ## [{c2, grapheme=GRAPHEME}, ???]:-
        	trigger(GRAPHEME, (GRAPHEME=q; GRAPHEME=k)).



      [{n, word=W}]                     
      ==>
      [{e, word=W}] :
      [???] ## [{c2, artic=ARTIC}:C1, ???]:-
         	(dentfric(C1); alvstop(C1); alvfric(C1); postalv(C1)).



   [{c0, grapheme=G0, alvstop, -emph, word=W}]                                  
   ==>
   [{c1, artic=D, grapheme= G1, word=W}]:
   [???] ## [{c2, artic=D, grapheme=G1}:C1, ???]:-
       	\+ G0 =  G1,
       	(dentfric(C1); alvstop(C1)).



  [{c0, dentfric, grapheme=G0, word=W}]                
  ==>
  [{c1, dentfric, grapheme=G1, word=W}]:
  [???] ## [{c2, dentfric, grapheme=G1}, ???]:-
       	\+ G0 =  G1.



   [{'D', word=W}] %%%GOOD      
   ==>
   [{'T', word=W} ] :
   [???] ## ['T', ???].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


  %  [{v0, artic=A},'A' , a, {alvlateral}, 'A', {shortV, -open, -rounded, word=WW}, {c0, grapheme=G, word=WWW}] %%% IF  'AL' WAS PROCEEDED BY A VOWEL %%%%% 
 %   ==>
 %   [{v1, shortV, artic=A, word=W}, {alvlateral, word=W1}, {shortV, -open, -rounded, word=WW}, {c1, grapheme=G, word=WWW} ] :
 %   [???] ## [ ???]:-
 %   	 +final:pos@W, -initial:pos@W,
 %   	 -final:pos@W1, +initial:pos@W1.


 % /**
 %  [{v0, artic=A},{'A', word=WWW} , a, {alvlateral}, {c1, grapheme=G, artic=AA,  +solar}]             
 %  ==>
 %  [{v1, shortV, artic=A, word=WW}, {c0, grapheme=G, artic= AA, +solar, word=W}, {c2, grapheme=G, artic=AA,  +solar, word=W1}] :
 %  [???] ## [???]:-
 % 	 +initial:pos@WWW,
 % 	 -final:pos@WW,
 % 	 + final:pos@W, -initial:pos@W,
 % 	 - final:pos@W1, +initial:pos@W1.
 %   **/			


 %  [{v0, artic=A},{'A', word=WWW} , a, {alvlateral}, {c1, grapheme=G, artic=AA,  +solar}]  %%% modified word boundary           
 %  ==>
 %  [{v1, shortV, artic=A, word=WW}, {c0, grapheme=G, artic= AA, +solar, word=W}, {c2, grapheme=G, artic=AA,  +solar, word=W1}] :
 %  [???] ## [???]:-
 % 	 +initial:pos@WWW,
 % 	 +final:pos@WW,
 % 	 -final:pos@W, +initial:pos@W,
 % 	 -final:pos@W1.

 %   				% +initial:pos@W.



 %  [{v0, shortV},{'A', word=WW} , a, {alvlateral}, {c1, grapheme=G, artic=AA,  +solar}]  %%% when preceeded bu prepositional           
 %  ==>
 %  [{v0, shortV}, {c0, grapheme=G, artic= AA, +solar, word=W}, {c2, grapheme=G, artic=AA,  +solar, word=W}] :
 %  [???] ## [???]:-
 %  	-initial:pos@WW,
 %  	- final:pos@W.

 % /**
 %   [{v0, artic=A}, {'A', word=WWW}, a, {alvlateral},  {c0, grapheme=G, -solar}]                   
 %   ==>
 %   [{shortV, artic=A, word=WW}, {alvlateral, word=W}, {c1, grapheme=G,  -solar, word=W1}] :
 %   [???] ## [???]:-
 % 	  +initial:pos@WWW,
 %  	 - final:pos@WW,
 %  	 + final:pos@W, -initial:pos@W,
 %  	 -final:pos@W1, +initial:pos@W1.
 % **/

 %   [{v0, artic=A}, {'A', word=WWW}, a, {alvlateral},  {c0, grapheme=G, -solar}]    %%%modofied               
 %   ==>
 %   [{shortV, artic=A, word=WW}, {alvlateral, word=W}, {c1, grapheme=G,  -solar, word=W1}] :
 %   [???] ## [???]:-
 % 	  +initial:pos@WWW,
 % 	  + final:pos@WW,
 % 	  - final:pos@W,+initial:pos@W,
 % 	  -final:pos@W1.


 %   [{v0, shortV}, {'A', word=WWW}, a, {alvlateral},  {c0, grapheme=G, -solar}]         %%% when preceeded bu prepositional             
 %   ==>
 %   [{v0, shortV, word=WW}, {alvlateral, word=WW}, {c1, grapheme=G,  -solar, word=WW}] :
 %   [???] ## [???]:-
 % 	  -initial:pos@WWW,
 %  	 - final:pos@WW.




 %  [{c0, grapheme=G}, {'A',word=W},a, {alvlateral}, 'A']           %%%%% IF  'AL' WAS PROCEEDED BY A CONSONANT %%%%%                 
 %  ==>
 %  [{c1,grapheme=G, word=WW}, {shortV, -open, -rounded, word= W1},  {alvlateral, word=W2}]:
 %  [???] ## [{shortV, -open, -rounded}, ???]:-
 %  	+initial:pos@W,
 %  	-final:pos@WW, -initial:pos@WW,
 %  	+final:pos@W1, -initial:pos@W1,
 %  	-final:pos@W2, +initial:pos@W2.

 % /**
 % [{c4, grapheme=GG}, {'A',word=W},a, l , {c0, grapheme=G, artic=AA, +solar}]                   
 % ==>
 % [{c5, grapheme=GG, word=WW}, {shortV, -open, -rounded, word= W3}, {c1, grapheme=G, artic=AA, +solar, word=W1}, {c2, grapheme=G, artic=AA, +solar, word=W2}]:
 % [???] ## [ ???]:-
 %  	-final:pos@WW,
 %  	-final:pos@W3,
 %  	+initial:pos@W,
 %  	+ final:pos@W1, -initial:pos@W1,
 %    	-final:pos@W2, +initial:pos@W2.
 % **/


 % [{c4, grapheme=GG}, {'A',word=W},a, l , {c0, grapheme=G, artic=AA, +solar}]    %%modified               
 % ==>
 % [{c5, grapheme=GG, word=WW}, {shortV, -open, -rounded, word= W3}, {c1, grapheme=G, artic=AA, +solar, word=W1}, {c2, grapheme=G, artic=AA, +solar, word=W2}]:
 % [???] ## [ ???]:-
 %  	-final:pos@WW,
 %  	+final:pos@W3,
 %  	+initial:pos@W,
 %  	- final:pos@W1, +initial:pos@W1,
 %    	-final:pos@W2.


 % /**
 % [{c3, grapheme=GG},{'A',word=W}, a,  {alvlateral}, {c0, grapheme=G, -solar}]
 % ==>
 % [{c4, grapheme=GG, word=W4},{shortV, -open, -rounded, word= W3}, {alvlateral, word=W1}, {c1, grapheme=G,  -solar, word=W2}]: 
 % [???] ## [???]:-
 % 	+initial:pos@W,
 % 	+ final:pos@W1, -initial:pos@W1,
 % 	-final:pos@W2, +initial:pos@W2,
 % 	-final:pos@W3,
 % 	-final:pos@W4.
 % **/

 % [{c3, grapheme=GG},{'A',word=W}, a,  {alvlateral}, {c0, grapheme=G, -solar}]  %modified
 % ==>
 % [{c4, grapheme=GG, word=W4},{shortV, -open, -rounded, word= W3}, {alvlateral, word=W1}, {c1, grapheme=G,  -solar, word=W2}]: 
 % [???] ## [???]:-
 % 	+initial:pos@W,
 % 	- final:pos@W1, +initial:pos@W1,
 % 	-final:pos@W2,
 % 	+final:pos@W3,
 % 	-final:pos@W4.


 %   [{'A', utt=U, word=W} ,  {v0, +open, -long}, {alvlateral, word=WW}, 'A'] %%%% IF 'AL'WAS AT THE BEGINNING OF THE UTTERANCE%%%%                         
 %   ==>
 %   [{'\'', word=W}, {v0, +open, -long},  {alvlateral, word=WW}] :
 %   [???] ## [{shortV, -open, -rounded}, ???]:-            
 %          	+initial:pos@U.
	


 %   [{'A', utt=U, word=W} , {v0, +open, -long},  {alvlateral, word=WW}]                         
 %   ==>
 %   [{'\'', word=W}, {v0, +open, -long}, {c0, grapheme=G, +solar, word=WW}] :
 %   [???] ## [{c1, grapheme=G, +solar}, ???]:-            
 %   	+initial:pos@U.


 %   [{'A', utt=U, word=W} , {v0, +open, -long}, {alvlateral, word=WW}]                
 %   ==>
 %   [{'\'', word=W} , {v0, +open, -long}, {alvlateral, word=WW}]:
 %   [???] ## [{c0, -solar}, ???]:-                              
 %         	+initial:pos@U.

