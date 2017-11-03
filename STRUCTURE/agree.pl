
%%%% AGREE.PL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/***

Common values for "agree" and "case". If you need extra combinations,
add them here rather than where you need them: this keeps things modular
and makes sure that you keep everything in step.
	
*/                         

/***************************************************************************/

count_noun(X) :-
    +individual@X,
    -kind@X,
    -mass@X.

mass_noun(X) :-
    -individual@X,
    -kind@X,
    +mass@X.

notmass_noun(X) :-
    -mass@X.

kind(X) :-
    -individual@X,
    +kind@X,
    -mass@X.

notkind(X) :-
    -kind@X.

not_singular(X) :-
    -singular:num@first@X,
    -singular:num@second@X,
    -singular:num@third@X.

not_dual(X) :-
    -dual:num@first@X,
    -dual:num@second@X,
    -dual:num@third@X.

not_plural(X) :-
    -plural:num@first@X,
    -plural:num@second@X,
    -plural:num@third@X.

blockedAgreement(person@X) :-
    X <> [not_plural, not_dual, not_singular].

singular(X) :-
    X <> [not_dual, not_plural].

dual(X) :-
    X <> [not_singular, not_plural].

plural(X) :-
    X <> [not_singular, not_dual].

not_first_sing(X) :-
    -singular:num@first@X.

not_first_dual(X) :-
    -dual:num@first@X.

not_first_plural(X) :-
    -plural:num@first@X.

not_first(X) :-
    X <> [not_first_sing, not_first_dual, not_first_plural].

not_second_sing(X) :-
    -singular:num@second@X.

not_second_dual(X) :-
    -dual:num@second@X.

not_second_plural(X) :-
    -plural:num@second@X.

not_second(X) :-
    X <> [not_second_sing, not_second_dual, not_second_plural].

not_third_sing(X) :-
    -singular:num@third@X.

not_third_dual(X) :-
    -dual:num@third@X.

not_third_plural(X) :-
    -plural:num@third@X.

not_third(X) :-
    X <> [not_third_sing, not_third_dual, not_third_plural].

first_sing(X, singular:num@first@X) :-
    +individual@X,
    -kind@X,
    -mass@X.

first_sing(X) :-
    X <> first_sing(+).
 
first(X) :-
    X <> [not_second, not_third, count_noun].

first_sing_only(X) :-
    +singular:num@first@X,
    -dual:num@first@X,
    -plural:num@first@X,
    X <> [first, singular].

first_plural(X) :-
    -singular:num@first@X,
    -dual:num@first@X,
    +plural:num@first@X,
    X <> count_noun.

first_plural_only(X) :-
    X <> [first_plural, first, plural].
    
second(X) :-
    X <> [not_first, not_third, count_noun].

second_sing(X) :-
    X <> [second, singular, count_noun],
    +singular:num@second@X.
    
second_sing_only(X) :-
    X <> [second_sing, second].
    
second_dual(X) :-
    -singular:num@second@X,
    +dual:num@second@X,
    -plural:num@second@X.
                      
second_dual_only(X) :-
    X <> [second_dual, second].
    
second_plural(X) :-
    X <> [not_first, plural, not_third, count_noun],
    +plural:num@second@X.
    
second_plural_only(X) :-
    X <> [second_plural, plural, second].

not_1_or_2_plural(X) :-
    -plural:num@first@X,
    -plural:num@second@X,
    X <> count_noun.

not_1_or_2pl(X) :-
    -singular:num@first@X,
    -dual:num@first@X,
    -plural:num@first@X,
    -plural:num@second@X,
    X <> count_noun.

not_1pl_or_2s_or_3(X) :-
    -plural:num@first@X,
    -singular:num@second@X,
    -singular:num@third@X,
    -dual:num@third@X,
    -plural:num@third@X,
    X <> count_noun.

third(X) :-
    X <> [not_first, not_second].

%not the plural of the 2nd or 3rd persons - kat(24.01.05)
not_2pl_or_3pl(X) :-
    -dual:num@second@X,
    -plural:num@second@X,
    -dual:num@third@X,
    -plural:num@third@X,
    X <> count_noun.

%only plural of 2nd or 3rd person
secondpl_or_thirdpl(X) :-
    X <> [not_first, plural, count_noun].

%only second(any) or thirdsing
second_or_thirdsing(X) :-
    X <> [not_first, count_noun],
    -dual:num@third@X,
    -plural:num@third@X.

not_1_plural(X) :-
    -dual:num@first@X,
    -plural:num@first@X,
    X <> count_noun.

not_2pl(X) :-
    -plural:num@second@X,
    X <> count_noun.

second_singular(X) :-
    X <> [second, singular, count_noun],
    +singular:num@second@X.

agree_3s_or_2pl(X) :-
    X <> [not_first],
    -singular:num@second@X,
    -dual:num@second@X,
    -dual:num@third@X,
    -plural:num@third@X,
    -kind@X,
    -mass@X.

third_or_2s(X) :-
    X <> not_first,
    -dual:num@second@X,
    -plural:num@second@X,
    -kind@X,
    -mass@X.

third_sing(X) :-
    +singular:num@third@X,
    -dual:num@third@X,
    -plural:num@third@X.
 
third_dual(X) :-
    -singular:num@third@X,
    +dual:num@third@X,
    -plural:num@third@X.
                      
third_dual_only(X) :-
    X <> [third, third_dual].

not2ndor3rdplural(X) :-
    -dual:num@second@X,
    -plural:num@second@X,
    -dual:num@third@X,
    -plural:num@third@X.
           
third_plural(X) :-
    -singular:num@third@X,
    -dual:num@third@X,
    +plural:num@third@X.
                
broken_third_plural(X) :-
    -plural:num@third@X.

third_sing_only(X) :-
    third(X), 
    third_sing(X), 
    singular(X).

third_plural_only(X) :-
    third(X), third_plural(X), plural(X).

firstOrThirdSing(X) :-
    singular:num@first@X <-> FIRSTSING,
    singular:num@third@X <-> THIRDSING,
    trigger(THIRDSING,
	    (FIRSTSING = - -> THIRDSING = +)).
    
isItThirdSing(X, singular:num@third@X).
   
somaliEmptyAgr(X) :-
    -plural:num@first@X,
    -singular:num@second@X,
    -plural:num@second@X,
    singular:num@third@X <-> THIRDSING,
    X <> [count_noun, not_dual],
    masculine@X <-> MASC,
    % When you know whether it's third sing, then if it actually is then
    % make sure it's also masculine
    trigger(THIRDSING,
	    (THIRDSING = + -> (MASC = +); true)).

somaliOtherWeirdAgr(X) :-
    X <> not_first,
    singular:num@third@X <-> THIRDSING,
    feminine@X <-> FEM,
    % When you know whether it's third sing, then if it actually is then
    % make sure it's also feminine
    trigger(THIRDSING,
	    (THIRDSING = + -> (FEM = +); true)).

third_singFeminine(X) :-
    -dual:num@third@X,
    -plural:num@third@X,
    singular:num@third@X <-> THIRDSING,
    feminine@X <-> FEM,
    trigger(THIRDSING, (THIRDSING = + -> (FEM = +); true)).

%not 3sgf or 1pl
somaliEmptyRestrictive(X) :-
    -plural:num@first@X,
    singular:num@third@X <-> THIRDSING,
    masculine@X <-> MASC,
    % When you know whether it's third sing, then if it actually is then
    % make sure it's also feminine
    trigger(THIRDSING,
	    (THIRDSING = + -> (MASC = +); true)).

%not 2nd (anything) or 3pl
somaliDivergentA(X) :-
    X <> not_first,
    -dual:num@third@X,
    -plural:num@third@X,
    X <> count_noun.

%MIGHT NEED THIS RULE SOMETIME
somaliEmptyAgr_prefix(X) :-
    -plural:num@first@X,
    -plural:num@second@X,
    -singular:num@third@X,
    -dual:num@third@X,
    X <> count_noun.
 
neuter(X) :-
    +neuter@X,
    -masculine@X,
    -feminine@X.

masculine(X) :-
    -neuter@X,
    +masculine@X,
    -feminine@X.

masculine(X, masculine@X).

notmasc(X) :-
    X <> masculine(-).

feminine(X) :-
    -neuter@X,
    -masculine@X,
    +feminine@X.

feminine(X, feminine@X).

notfem(X) :-
    X <> feminine(-).

gendered(X) :-
    -neuter@X.

shortGender(X, _) :-
    \+ ground(gender@X),
    !.
shortGender(X, masculine) :-
    X <> masculine,
    !.
shortGender(X, feminine) :-
    X <> feminine,
    !.
shortGender(_X, unknown).

/*


We currently allow for up to five morphologically marked cases: this is more

than we need for some languages (e.g. English), and may be less than we need for

others.


nominative: semantically vacuous cases always associated with the subject

dative: semantically meaningful cases depicting destination or recipient.
Corresponds to the preposition "to" in English.

genitive: semantically meaningful cases depicting possession or very close
relationship (part-whole, ...).  Corresponds to the preposition "of" in English.

vocative: means "I'm talking to you". In something like "John, you come
here now" the NP "John" would be in vocative.

ablative: (which we don't have, but we could need) means something like
source, and would correspond roughly to "from"

locative: (which we don't have, but maybe we should) would mean something about
place.

accusative: none of the above

The middle ones all tend to correspond in certain circumstances to proper
PPs. What we're looking for is morphological case markers that co-distribute
with particular (types of) PP


*/

nom(X, nom@X).

nom(X) :-
    +nom@X.
   
nomvoc(X) :-
    -acc@X,
    -dat@X,
    -gen@X,
    -pcase@X.
 
nomOrPrep(X) :-
    -acc@X,
    -dat@X,
    -gen@X,
    -voc@X.

accOrPrep(X) :-
    -nom@X,
    -dat@X,
    -gen@X,
    -voc@X.

nomOrAcc(X) :-
    -dat@X,
    -gen@X,
    -voc@X,
    -pcase@X.

notnom(X) :-
    -nom@X.

reallyNom(X) :-
    +nom@X,
    -acc@X,
    -dat@X,
    -gen@X,
    -voc@X,
    -pcase@X.
    
acc(X) :-
    -nom@X,
    +acc@X,
    -dat@X,
    -gen@X,
    -voc@X,
    -pcase@X.

acc_or_dat(X) :-
    -nom@X,
    -gen@X,
    -voc@X,
    -pcase@X.

acc_or_prep(X) :-
    -nom@X,
    -dat@X,
    -gen@X,
    -voc@X.

notcasemarked(X) :-
    -nom@X,
    -acc@X,
    -dat@X,
    -gen@X,
    -voc@X,
    -pcase@X.

not_nom_acc(X) :-
    -nom@X,
    -acc@X.
     
not_acc(X) :-
    -acc@X.
    
not_nom_gen(X) :-
    -nom@X,
    -gen@X.
    
not_nom_voc(X) :-
    -nom@X,
    -voc@X.

gen(X) :-
    -nom@X,
    -acc@X,
    -dat@X,
    +gen@X,
    -voc@X,
    -pcase@X.

notgen(X) :- 
    -gen@X.
    
dat(X) :-
    -nom@X,
    -acc@X,
    +dat@X,
    -gen@X,
    -voc@X,
    -pcase@X.

notdat(X) :- 
    -dat@X.

voc(X) :-
    -nom@X,
    -acc@X,
    -dat@X,
    -gen@X,
    +voc@X,
    -pcase@X.

notvoc(X) :- 
    -voc@X.

not_dat_gen(X) :- 
    -dat@X,
    -gen@X.
    
not_gen_voc(X) :- 
    -nom@X,
    +acc@X,
    -dat@X,
    -gen@X,
    -voc@X,
    -pcase@X.

acc(X, acc@X).

no_case_default(X) :-
    -caseDefault@X.

pcase(X) :-
    -nom@X,
    -acc@X,
    -dat@X,
    -gen@X,
    -voc@X,
    +pcase@X.

pcase(X, pform@X) :-
    X <> pcase.

case_marked(X, acc) :-
    -nom@X,
    +acc@X,
    -dat@X,
    -gen@X,
    -voc@X,
    -pcase@X.

case_marked(X, dat) :-
    -nom@X,
    -acc@X,
    +dat@X,
    -gen@X,
    -voc@X,
    -pcase@X.

case_marked(X, gen) :-
    -nom@X,
    -acc@X,
    -dat@X,
    +gen@X,
    -voc@X,
    -pcase@X.
    
case_marked(X) :-
    case_marked(X, _).

not_case_marked(X) :-
    -voc@X,
    -pcase@X.

% clauses for adj agreement in german (generic has been omitted for the time being)

definite1(X) :-
    +definite@X,
    -indefinite@X.
    
indefinite1(X) :-
     -definite@X,
     +indefinite@X.

construct(X) :-
    +definite@X.

der_word(X) :-
    +definite@X,
    -indefinite@X,
    -generic@X.

ein_word(X) :-
     -definite@X,
     +indefinite@X,
     -generic@X.

zero_word(X) :-
    -definite@X,
    -indefinite@X,
    +generic@X.

not_der_word(X) :-
     -definite@X.

not_zero_word(X) :-
     -generic@X.

agree(X1, X2) :-
    agree@X1 <-> agree@X2.
