
%%%% TAGS.PL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% verbs

tag('VV', verb).
tag('AUXBE', beAux).
tag('COPBE', beCop).
tag('DOTRANS', doTrans).
tag('DOAUX', doAux).
tag('HAVETRANS', haveTrans).
tag('HAVEAUX', haveAux).
tag('HAVETO', haveTo).
tag('COMP', toComp).
tag('TO', to).
tag('VM', modal(_MODAL)).

tag('I', infinitive).
tag('B', presTense).
tag('G', presPart).
tag('D', pastTense).
tag('N', pastPart).
tag('P', passivePart).

%% nouns
tag('NN', noun).
tag('ND', date).
tag('1', singNoun).
tag('2', pluralNoun).

%% determiners
tag('DT', much(1)).
tag('NN', much(2)).
tag('ALL', all(2)).
tag('MORE2', more(2)).
tag('MORE3', more(3)).
tag('MOST', most(1)).
tag('DT', many).
tag('DT', det).

%% pronouns
tag('PN', pronoun).
tag('PW', whpronoun).
tag('A', acc).
tag('N', nom).
tag('X', other).

%% adjectives
tag(TAG, adj2) :- tag(TAG, adjective).
tag('AJ', adjective).
tag('AV', adverb).

tag('B', simpleAdj).
tag('C', comparative).
tag('S', superlative).

%% prepositions

tag('OVER3', over(3)).
tag('PTCLE', particle).
tag('PP', prep).
tag('0', prep0).
tag('1', prep1).

%% WH-items
tag('HOWMOD', howMod).
tag('HOWPP', howPP).
tag('WHMOD', where).
tag('WHMOD', when).
tag('WHMOD', why).
tag('WHMOD', while).
tag('WHMOD', whether).
tag('WHDET', whatDet).
tag('WHDET', whichDet).

tag('PRO', thatPRO).
tag('WHPRO', who).
tag('WHPRO', whatWHPRO).
tag('WHPRO', whichWHPRO).
tag('WHPRO', thatWHPRO).
tag('COMP', thatCOMP).
tag('THATDET', thatDET).

tag('WHETHER', ifWhether).

%% misc
tag('CNJ1', conj1).
tag('CNJ2', conj2).
tag('CONN', connective).
tag('SO1', so1).
tag('SO2', so2).
tag('NUM', number).
tag('NOT', not(_)).
tag('POSS', poss).

%% punctuation
tag('STOP', fullStop).
tag('STOP', exclMark).
tag('QUERY', query).
tag('COMMA1', comma(1)).
tag('COMMA2', comma(2)).
tag('HYPHEN', dash(0)).
tag('DASH1', dash(1)).

