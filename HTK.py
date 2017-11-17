#!/usr/bin/python

import subprocess
import os
import shutil
import re
import math
import sys
import random
import dtw
reload(dtw)
import threading
import time
import a2bw
reload(a2bw)
from shared import *
import segments
reload(segments)
import buck

try:
    pya
    reload(pya)
except:
    import pyaramorph as pya
    reload(pya)
from useful import *
from datetime import datetime

DEBUG = 0

##################################################################
#                    General utilities                           #
##################################################################

DEBUG = 0

QUIETLY = True

space = re.compile("\s+")

def singlespaces(s):
    return space.sub(" ", s)

def clean(d, targets=["data", "training", "genfiles"]):
    print "clean(targets=%s)"%(targets)
    allfiles = os.listdir(d)
    if isinstance(targets, str):
        targets = [targets]
    files = {"data": map(re.compile, ['allprompts.txt', 'monophones.*', 'phones..mlf', 'testing.scp', 'training.txt', 'config.txt', 'mfc.scp', 'phonprompts.txt', 'testing.txt', 'mfcconfig.txt', 'prompts.pl', 'training.*.scp', 'words.mlf']),
             "wav": map(re.compile, ["wav", 'mfc*']),
             "genfiles": map(re.compile, ['allprompts.txt', 'monophones.*', 'phones..mlf', 'testing.scp', 'training.txt', 'config.txt', 'mfc.scp', 'phonprompts.txt', 'testing.txt', 'mfcconfig.txt', 'prompts.pl', 'training.scp', 'words.mlf', 'wdnet']),
            "training": map(re.compile, ['aligned.mlf', 'hmm.*', '.*acc', 'tiedlist', 'train.scp', 'wintri.mlf', 'dict-tri', 'mktri.hed', 'proto.txt', 'mktri.led', 'flog', 'sil.hed', 'tree.hed', 'fulllist', 'stats', 'trees', 'gram.txt', 'triphones1'])}
    for target in targets:
        for t in files[target]:
            for f in allfiles:
                if t.match(f):
                    try:
                        os.remove(os.path.join(d, f))
                    except:
                        try:
                            shutil.rmtree(os.path.join(d, f))
                        except:
                            pass

lengthpattern = re.compile("Length\s*\(seconds\):\s*(?P<length>\S+)", re.DOTALL)
def wavlength(fname):
    if os.path.isdir(fname):
        t = 0.0
        for i, w in enumerate(os.listdir(fname)):
            if w.endswith(".wav"):
                t += wavlength(os.path.join(os.getcwd(), fname, w))
        return t
    else:
        x = subprocess.Popen(("sox %s -n stat"%(fname)).split(), stdout=subprocess.PIPE, stderr=subprocess.PIPE).communicate()
        return float(lengthpattern.search(x[1]).group('length'))

##################################################################
# Lets you do experiments with 0 & 1 derivatives and 0, 1 & 2   #
##################################################################  

MFCCONFIG25 = """
SOURCEFORMAT = WAV
TARGETKIND = MFCC_0
TARGETRATE = 100000.0
SAVECOMPRESSED = T
SAVEWITHCRC = T
WINDOWSIZE = 250000.0
USEHAMMING = T
PREEMCOEF = 0.97
NUMCHANS = 26
CEPLIFTER = 22
NUMCEPS = 12 
ENORMALISE = F
"""

CONFIG25 = """
TARGETKIND = MFCC_0_D_N_Z
TARGETRATE = 100000.0
SAVECOMPRESSED = T
SAVEWITHCRC = T
WINDOWSIZE = 250000.0
USEHAMMING = T
PREEMCOEF = 0.97
NUMCHANS = 26
CEPLIFTER = 22
NUMCEPS = 12 
"""

PROTO25 = """~o <VecSize> 25 <MFCC_0_D_N_Z>
~h "proto"
<BeginHMM>
  <NumStates> 5

  <State> 2

    <Mean> 25
      0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 
    <Variance> 25
      1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 
      
  <State> 3

    <Mean> 25
      0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 
    <Variance> 25
      1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 
      
  <State> 4

    <Mean> 25
      0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 
    <Variance> 25
      1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 
      
 <TransP> 5
  0.0 1.0 0.0 0.0 0.0
  0.0 0.6 0.4 0.0 0.0
  0.0 0.0 0.6 0.4 0.0
  0.0 0.0 0.0 0.7 0.3
  0.0 0.0 0.0 0.0 0.0
<EndHMM>
"""

MFCCONFIG39 = """# Coding parameters
SOURCEFORMAT = WAV
TARGETKIND = MFCC_0
TARGETRATE = 100000.0
SAVECOMPRESSED = T
SAVEWITHCRC = T
WINDOWSIZE = 250000.0
USEHAMMING = T
PREEMCOEF = 0.97
NUMCHANS = 26
CEPLIFTER = 22
NUMCEPS = 12
ENORMALISE = F
"""

CONFIG39 = """
TARGETKIND = MFCC_0_D_A_Z
TARGETRATE = 100000.0
SAVECOMPRESSED = T
SAVEWITHCRC = T
WINDOWSIZE = 250000.0
USEHAMMING = T
PREEMCOEF = 0.97
NUMCHANS = 26
CEPLIFTER = 22
NUMCEPS = 12
ENORMALISE = F
"""

PROTO39 = """~o <VecSize> 39 <MFCC_0_D_A_Z>
~h "proto"
<BeginHMM>
<NumStates> 5
<State> 2
<Mean> 39
0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 
<Variance> 39
1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0
<State> 3
<Mean> 39
0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
<Variance> 39
1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0
<State> 4
<Mean> 39
0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
<Variance> 39
1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0
<TransP> 5
    0.0 1.0 0.0 0.0 0.0
    0.0 0.6 0.4 0.0 0.0
    0.0 0.0 0.6 0.4 0.0
    0.0 0.0 0.0 0.7 0.3
    0.0 0.0 0.0 0.0 0.0
<EndHMM>
"""

def useconfig25():
    global MFCCONFIG, CONFIG, PROTO
    MFCCONFIG, CONFIG, PROTO = MFCCONFIG25, CONFIG25, PROTO25

def useconfig39():
    global MFCCONFIG, CONFIG, PROTO
    MFCCONFIG, CONFIG, PROTO = MFCCONFIG39, CONFIG39, PROTO39

def htkformat_dictionary(dict, exptdir, dictfile, D=0, useFixedDict="multi"):
    print "htkformat_dictionary(%s, %s)"%(exptdir, dictfile)
    if D == 1:
        sp = " sp"
    else:
        sp = ""
    m = 0
    with safeout(os.path.join(exptdir, dictfile)) as write:
        for key in sorted(dict.iterkeys()):
            if key != '!ENTER' and key != '!EXIT' and key != 'silence': # to prevent putting their vales ('sil') as phones 
                entries = sorted(dict[key])
                if len(entries) > 1:
                    m += 1
                for value in entries:
                    write('%s\t%s%s\n'%(key, value, sp))
                    if useFixedDict == "fixed" or D == 0:
                        break
            else:
                write('%s\t%s\n'%(key, 'sil'))
    if D == 1:
        print "%s words out of %s had multiple entries"%(m, len(dict))
    

##################################################################
#                        Prolog stuff                            #
##################################################################

    
def gen_prologPrompts(exptdir, prompts="allprompts.bwf", prologprompts='prompts.pl', N=sys.maxint):
    print "gen_prologPrompts(%s)"%(exptdir)
    d, p = makePYADict(exptdir, prompts)
    f = [line.strip() for line in open("%s/%s"%(exptdir, prompts), 'r').readlines() if not line.strip() == ''][:N]
    prologPrompts = "["
    for line in f:
        patt = '(.+) !ENTER (.+) !EXIT' # re pattern - 1st group (.+)= sample names | 2nd group (.+)= all words in a sample 
        sampleNum = re.match(patt,line).group(1)
        sentence = re.match(patt,line).group(2)
        sentence = " ".join([singlespaces(getBest(d[w])[0]) for w in sentence.split()])
        prologPrompts = prologPrompts + "sentence(\"" + sampleNum+ "\", \"" + sentence + "\"),\n"   
    prologPrompts = prologPrompts[:-2] + "]."
    with safeout("%s/%s"%(exptdir, prologprompts)) as write:
        write(prologPrompts)

def genPrologPromptsFromPhones(dest, phones="phones1.mlf", prologprompts='prompts.pl', N=sys.maxint):
    sPattern = re.compile('"(?P<fname>\S*).lab"\s*(?P<phones>[^\.]*)', re.DOTALL)
    s = "["
    for i, sentence in enumerate(sPattern.finditer(open(os.path.join(dest, phones)).read())):
        if i == N:
            break
        phones = sentence.group("phones").replace("sil\n", "").replace("sp", " ").replace("\n", "")
        if not phones == '':
            s += 'sentence("%s", "%s"),\n'%(sentence.group("fname"), phones)
    s = s[:-2]+"].\n"
    with safeout(os.path.join(dest, prologprompts)) as write:
        write(s)
    
def runprolog(d, prompts='prompts.pl', phonprompts="phonprompts.txt", useProlog=1):
    HARMONY = "/Users/ramsay/ASIM/IMAN/HARMONY-05-10-11"
    print HARMONY
    prolog = ["sicstus", "-r", "%s/test.sav"%(HARMONY), "-a", d, prompts, phonprompts, str(useProlog)]
    print prolog
    print "Running phonological rules--takes a little while, can't print progress messages because we're capturing the output"
    x = execute(prolog)
    print "Done: %s"%(x[1])
        
def noEmptyStrings(l):
    return [x for x in l if not x == '' and not x == ' + ']
  
def genDictFromProlog(exptdir, phonprompts="phonprompts.txt", allprompts="allprompts.bwf"):
    print "genDictFromProlog('%s', '%s', '%s')"%(exptdir, phonprompts, allprompts)
    allprompts = {p.split()[0]: p for p in open(allprompts)}
    data = open("%s/%s"%(exptdir, phonprompts)).read()
    dict = {}
    patt = re.compile("'(?P<prompt>\S*)'\ntext: (?P<text>[^\n]*)\nsampa: (?P<sampa>[^\n]*)\n", re.DOTALL)
    for it in patt.finditer(data):        
        words = it.group('text').strip() # patt = 'text: (.+?) ...' ==> group(1), or patt = 'text: (?P<text>.+?) ...' ==> group('text') 
        prompt = allprompts[it.group("prompt")]
        wordslist = prompt.split()[2:-1] # some words in one line,, changes from double spaces to single space
        phones = it.group('sampa').strip() 
        netPhones = noEmptyStrings(re.split('##',phones)) # to remove '' from phones
        if not len(wordslist) == len(netPhones):
            printall(zip(wordslist, netPhones))
            raise Exception('Wrong number of words for phone sets: %s'%(it.group("prompt")))
        for i in range(0, len(wordslist)):
            word = wordslist[i]
            transcription = netPhones[i].strip()
            if not word in dict:
                dict[word] = {}
            try:
                dict[word][transcription] += 1
            except:
                dict[word][transcription] = 1
    dict['!ENTER'] = {'sil':1}
    dict['!EXIT'] = {'sil':1}
    dict['silence'] = {'sil':1}
    return dict
    
def prologprep(exptdir, allprompts='allprompts.bwf', prologprompts="prompts.pl", phonprompts="phonprompts.txt", useProlog=1, useFixedDict='multi', N=sys.maxint):
    print "prolog('%s', '%s', allprompts='%s', prologprompts='%s', useProlog=%s)"%(exptdir, allprompts, prologprompts, phonprompts, useProlog)
    """ Get the original prompts into a Prolog-friendly format """
    genPrologPromptsFromPhones(exptdir, phones="phones1.mlf", prologprompts=prologprompts, N=N)
    """
    Run Iman's phonological rules to generate a set of prompts. Once we've done this we have a
    contextually determined set of prompts
    """
    runprolog(exptdir, prompts=prologprompts, phonprompts=phonprompts, useProlog=useProlog)
    """
    Use the prompts you just generated to create a dictionary. This may be 
    multi-entry dictionary: this will happen if any of the phonological rules
    applied across word boundaries, so (a) you have to have rules that *can*
    apply across word boundaries and (b) there have to have been occurrences
    of words in contexts that differ in such a way as to cause those rules
    to apply. So for the simpler rules or for small training sets it's quite
    likely that the dictionary will only have a single entry per word.
    """
    return genDictFromProlog(exptdir=exptdir, phonprompts=phonprompts, allprompts=os.path.join(exptdir, allprompts))
    

####################################################################################
# General initialisation stuff: copy wav files & prompts, make .scp and .mlf files #
####################################################################################

# Anyword grammar
def makeGrammar1(d, prompts="allprompts.txt", dfile=False, gram="gram.txt"):
    pattern = re.compile(".*!ENTER (?P<words>.*?) !EXIT.*")
    words = set()
    if isinstance(prompts, str):
        prompts = [prompts]
    for promptfile in prompts: 
        for p in pattern.finditer(open("%s/%s"%(d, promptfile)).read()):
            for w in p.group("words").split(" "):
                words.add(w)
    with safeout("%s/%s"%(d, gram)) as write:
        write("""
$WORDS = (%s);
(!ENTER <$WORDS> !EXIT)
"""%(" | ".join(sorted(words))))
    execute('HParse %s/gram.txt %s/wdnet'%(dest, dest))

# Fixed sentence grammar
def makeGrammar2(d, prompts="prompts.txt", dfile=False, gram="gram.txt"):
    pattern = re.compile(".*!ENTER (?P<words>.*?) !EXIT.*")
    sentences = set()
    if isinstance(prompts, str):
        prompts = [prompts]
    for promptfile in prompts:
        for p in pattern.finditer(open("%s/%s"%(d, promptfile)).read()):
            sentences.add(p.group("words"))
    with safeout("%s/%s"%(d, gram)) as write:
       write("""
$SENTENCE = (%s);
(!ENTER $SENTENCE !EXIT)
"""%("\n | ".join(sentences)))
    execute('HParse %s/gram.txt %s/wdnet'%(dest, dest))

def makeBigramGrammar(d, prompts, dfile="dict0.txt"):
    words = set()
    with safeout(os.path.join(d, "wlist")) as write:
        for prompt in prompts:
            for w in prompt[1:]:
                w = w[FORM]
                if not w in words:
                    write(w+"\n")
                    words.add(w)
    execute("HLStats -b %s/bigfn -o %s/wlist %s/words.mlf"%(d, d, d))
    execute("HBuild -n %s/bigfn %s/wlist %s/wdnet"%(d, d, d))

def readPrompts(prompts):
    if not "\n" in prompts:
        prompts = open(prompts).read().strip()
    return [singlespaces(p.strip()) for p in prompts.split("\n")]

"""
The raw Mada output file looks like (I've had to remove all the actual
Arabic in order to get this allowed inside a Python source file)

;;; SENTENCE ...
;;WORD ...
;;LENGTH 5
;;OFFSET 0
;;SVM_PREDICTIONS: ... diac:minoTaqapi lex:minoTaqap asp:na cas:n enc0:0 gen:f mod:na num:s per:na pos:noun prc0:0 prc1:0 prc2:0 prc3:0 stt:c vox:na
*0.883957 diac:... lex:... bw:minoTaq/NOUN+ap/NSUFF_FEM_SG+u/CASE_DEF_NOM gloss:area;zone;territory sufgloss:[fem.sg.] pos:noun prc3:0 prc2:0 prc1:0 prc0:0 per:na asp:na vox:na mod:na gen:f num:s stt:c cas:n enc0:0 rat:y source:lex stem: stemcat:Napdu
--------------
...

If we split it on "SENTENCE" we will get a list of sentences, each of
which consists of a set of WORDs. Each of these contains (at least
one) bit that looks like *0.883957 diac:.... So if we iterate over a
pattern that looks for a sequence of digits (i.e. the last bit of the
number) followed by "diac:" followed by some stuff, then "some stuff"
will be the diacriticised form.
"""

class MADASOLUTION:

    def __init__(self, form, diacritics, gloss):
        self.form = form
        self.diacritics = diacritics
        self.gloss = gloss

    def __repr__(self):
        return "%s:%s:%s"%(str(self.form.encode("UTF-8")), str(self.diacritics), str(self.gloss))

"""
Copy the diacritics from the following word. If you hit the next
element of the form of the preceding one, move on. If this was the end
of the preceding one then quit.
"""

def borrowDiacritics(w0, w1):
    d = u""
    b = buck.uni2buck(w0.form)
    for i, c in enumerate(w1.diacritics):
        d += c
        if c == b[0]:
            b = b[1:]
            if b == "":
                if len(d) == 1:
                    try:
                        d += w1.diacritics[i+1]
                    except:
                        pass
                break
    return d

"""
Regarding the dash with assimilation rules, here is the proposal:
Words with dash can be assimilated with the previous word and the
following word.  It may be assimilated with the previous word if it
starts with a definite article. In most cases, speakers tend not to
assimilate the definite article with the previous word when they
pronounce it solely (Al-). So, we shouldn't apply the assimilation
rules. However, the assimilation rules are not applicable on such case
because in "readRawMada" we mentioned that (Al-) is transcribed as
(Qal) and the assimilation rules don't apply on the glottal stop. The
other case where we have a definite article and part of the word, the
assimilation rules must be applied.

I listened to many wave files and I noticed that in most cases
speakers assimilate words with dashes with the following words. So, It
would be more reasonable to apply them. We can do this in two ways:
applying the assimilation rules and keeping the dash in the phonetic
transcription as a phoneme to indicate for the pausing or
aspiration. The other way is to delete the dashes when preprocessing
the text before applying the rules. I understand that you want to keep
the dashes in the phonetic transcription but in such case we will have
troubles in syllabifying the text and applying the stress rules. For
this reason, I preferred to remove the dashes from the phonetic
transcription.
"""
def fixDashes(sentence0):
    sentence1 = []
    n = 0
    for i, w in enumerate(sentence0):
        if w.form == "-":
            try:
                w0, w1 = sentence[i-1], sentence[i+1]
                n += 1
                """
                We're interested in the word before the dash. If that was "Al" 
                we will just fix the diacritics to be "Qal", otherwise we will
                try to borrow them from the next word
                """
                if w0.diacritics == "Al":
                    w0.diacritics = "Qal"
                elif sentence[i+1].form.startswith(sentence[i-1].form):
                    """ 
                    bgt- bgtdq
                    Mada said that bgt was bagat but that bgtdq was bugutidaq
                    Because the false start looks like the next word, we use the next word's
                    diacritics, so for bgt we do bugut
                    """
                    w0.diacritics = borrowDiacritics(w0, w1)
                elif len(sentence[i-1].form) == 1:
                    w0.diacritics = sentence[i-1].form+"a"
            except Exception as e:
                pass
            """
            Whatever happens, we will omit the dash from the transcription
            """
            continue
        else:
            sentence1.append(w)
    return sentence1

class foreign(Exception):

    def __init__(self, msg):
        self.msg = msg

    def __repr__(self):
        return "foreign('%s')"%(self.msg)
     
SPATTERN = re.compile(";;; SENTENCE(?P<sentence>.*?)SENTENCE BREAK", re.DOTALL)
WPATTERN = re.compile("WORD (@@LAT@@)?(?P<word>\S*)\s.*?--------------", re.DOTALL)
DPATTERN = re.compile(".*SVM_PREDICTIONS:\s*(@@LAT@@)?\S*\s*diac:(?P<diac>\S*)\s.*?((?P<gloss>gloss:\s*\S*)|(?P<none>NO-ANALYSIS)).*", re.DOTALL)


def readRawMada(ifile="TEMP/originalprompts.segments.mada", N=sys.maxint, pattern=False, wavfiles=False, wavtime=False, grain=1):
    sentences = []
    if os.path.isdir(ifile):
        for p, d, files in os.walk(ifile):
            for f in files:
                N, l = readRawMada(os.path.join(p, f), N=N, pattern=pattern, wavfiles=wavfiles, wavtime=wavtime, grain=grain)
                sentences += l
                if N <= 0:
                    return N, sentences
        return N, sentences
    else:
        if ifile.endswith(".mada"):
            for i, sentence in enumerate(SPATTERN.finditer(codecs.open(ifile, encoding="UTF-8").read().strip())):
                """
                Get all examples of the pattern, dig out the bit we want
                """
                sentence = sentence.group("sentence")
                if pattern and not pattern.match(sentence):
                    continue
                if N <= 0:
                    return N, sentences
                words = []
                if i%grain == 0:
                    try:
                        for w in WPATTERN.finditer(sentence):
                            form = w.group("word")
                            d = DPATTERN.match(w.group(0))
                            diac = d.group("diac")
                            try:
                                gloss = d.group("gloss")
                            except:
                                gloss = d.group("none")
                            if "foreign" in form:
                                raise foreign(form)
                            if re.compile(".*\d.*").match(form) and not form.startswith("*/test"):
                                raise foreign(form)
                            words.append(MADASOLUTION(form, diac, gloss))
                    except foreign:
                        continue
                    if len(words) > 3:
                        N -= 1
                        sentences.append(fixDashes(words))
    return N, sentences

"""
If I've read N for training and MAXTEST for testing and I should take
1 in MAXTEST/(MAXTEST+N) (e.g. MAXTEST=100, N=500, I've got 600 so I
should take 1 in 6). So grain = MAXTEST/len(filteredprompts)
"""
MAXTEST = 100
def splitPromptsToTrainAndTest(dest, filteredprompts, training="training.txt", testing="testing.txt", savetestset=True):
    training = os.path.join(dest, training)
    testing = os.path.join(dest, testing)
    try:
        os.remove(training)
    except:
        pass
    if not savetestset or not os.path.exists(testing):
        try:
            os.remove(testing)
        except:
            pass
        testing = open(testing, "w")
    else:
        print "USING EXISTING TESTSET"
        testing = False
    training = open(training, "w")
    j = 0
    grain = int(len(filteredprompts)/MAXTEST)
    for i, prompt in enumerate(filteredprompts):
        prompt = " ".join([prompt[0], prompt[1]]+HTKFriendly(prompt[2:-1])+[prompt[-1]])
        if j < MAXTEST and i % grain == 0:
            if testing:
                testing.write("%s\n"%(prompt))
            j += 1
        else:
            training.write("%s\n"%(prompt))
    if testing:
        testing.close()
    training.close()

promptPattern = re.compile('.*(?P<P>test\S*) .*')
def prompts2code(dest, prompts, train, wav="wav", mfcdir="mfc"):
    print "prompts2code(%s, %s, %s)"%(dest, prompts, train)
    with safeout(os.path.join(dest, train)) as write:
        for prompt in promptPattern.finditer(open(os.path.join(dest, prompts)).read()):
            prompt = prompt.group('P')
            write('"%s/%s/%s.mfc"\n'%(dest, mfcdir, prompt))
            
def wordsmlf(d, prompts):
    with safeout(os.path.join(d, 'words.mlf')) as write:
        write("#!MLF!#\n")
        for prompt in prompts:
            write(""""%s.lab"
%s
.
"""%(prompt[0][FORM], "\n".join(w[FORM] for w in prompt[1:])))
     
def phonprompts2phones(exptdir, prompts, training, phonprompts, n=0):
    print "phonprompts2phones(%s, %s, %s, %s, %s)"%(exptdir, prompts, training, phonprompts, n)
    phones = set(["sil"])
    prompts = {p.split()[0]: p for p in open(os.path.join(exptdir, prompts))}
    tprompts0 = {prompt.strip().split()[0]: prompt for prompt in open("%s/%s"%(exptdir, training)).read().split("\n") if not prompt.strip() == ""}
    p = re.compile("""'(?P<prompt>\S*)'
text: [^\n]*
sampa: (?P<sampa>[^[\n]*)
""", re.DOTALL)
    tprompts1 = []
    with safeout("%s/phones%s.mlf"%(exptdir, n)) as write:
        write("""#!MLF!#""")
        for i in p.finditer(open(os.path.join(exptdir, phonprompts)).read()):
            prompt = i.group("prompt")
            sampa = i.group("sampa")
            sampa = sampa.replace(" ## ", " sp " if n == 1 else " ")
            for phone in sampa.split(" "):
                phones.add(phone)
            if prompt in tprompts0:
                tprompts1.append(prompt)
                s = """
"%s.lab"
sil
%s
sil
.
"""%(prompt, sampa)
                write(re.compile("\s+").sub("\n", s))
    with safeout("%s/monophones%s"%(exptdir, n)) as write:
        for phone in phones:
            write("%s\n"%(phone))
    with safeout("%s/%s"%(exptdir, training)) as write:
        for prompt in tprompts1:
            write("%s\n"%tprompts0[prompt])
    
def baseprompts2phones(exptdir, prompts, training, dicTest, n=0):
    print "baseprompts2phones(%s, %s, %s, %s)"%(exptdir, prompts, training, n)
    phones = set(["sil"])
    if n > 0:
        phones.add("sp")
    prompts = readPrompts(os.path.join(exptdir, prompts))
    training = set([prompt.strip() for prompt in open("%s/%s"%(exptdir, training)).read().strip().split("\n")])
    xpattern = re.compile("(?P<testID>\*/\S*) !ENTER (?P<testTXT>.*) !EXIT")
    sp = "\n" if n == 0 else "\nsp\n"
    with safeout("%s/phones%s.mlf"%(exptdir, n)) as write:
        write("""#!MLF!#""")
        for tprompt in prompts:
            if tprompt in training:
                m = xpattern.match(tprompt)
                tprompt = m.group("testTXT").split()
                prompt = ""
                for i, w in enumerate(tprompt):
                    phonelist = sorted(dicTest[w])
                    prompt += "\n".join(phonelist[0].split())
                    if i < len(tprompt)-1:
                        prompt += sp
                    for k in phonelist:
                        for p in k.split():
                            phones.add(p)
                write("""
"%s.lab"
sil
%s
sil
.
"""%(m.group("testID"), prompt))
    with safeout("%s/monophones%s"%(exptdir, n)) as write:
        for phone in phones:
            write("%s\n"%(phone))
            
def makeBaselineDict(src="TEMP", dest="EXPT", prompts="originalprompts.segments.mada", useMada=3, useBW=False, sep="\n", N=sys.maxint, pattern=False):
    phondict = {}
    for prompt in prompts:
        for word in prompt.split(" ")[2:-1]:
            transcription = " ".join(word)
            if not word in phondict:
                phondict[word] = {}
            try:
                phondict[word][transcription] += 1
            except:
                phondict[word][transcription] = 1
    phondict['!ENTER'] = {'sil':1}
    phondict['!EXIT'] = {'sil':1}
    return phondict, {}

"""
For words that are not found by Pyaramorph, I insert "sv" between
adjacent consonants. It would be good to do something nre sensible,
but I have no idea how you would do that. I have done an experiment
where I did this with all words, and the results were significantly
worse than using what PyAramorph gives us when it gives us anything.

It would be worth getting fixsemivowels right. At the moment it's
pretty poor, and applying it leads to a very slight decrease in
accuracy, but if I got it right then I'm pretty sure it would help.

Allan, 26/07/2017
"""

def getPYAsolutions(htkfword, word, phondict, useMada):
    solution = False
    for solution in pya.getSolutions(word):
        solution.buckvoc = solution.buckvoc.replace("{", "A")
        solution = " ".join(HTKFriendly(solution.buckvoc.strip()))
        solution = fixPhonTrans(solution, useMada)
        if not solution in phondict[htkfword]:
            phondict[htkfword].append(solution)
    return solution

simpleprefix = re.compile("(?P<prefix>(Al)?)A(?P<rest>.*)")
def makePYADict(src, dest, prompts, useMada, N=sys.maxint, pattern=False):
    print "makePYADict: useMada=%s"%(useMada)
    phondict = {}
    pyadict = {}
    for prompt in prompts:
        for word in prompt[2:-1]:
            word = buck.uni2buck(word.form)
            htkfword = HTKFriendly(word)
            if htkfword in phondict:
                continue
            phondict[htkfword] = []
            solution = False
            s1 = False
            s2 = False
            if useMada:
                solution = getPYAsolutions(htkfword, word, phondict, useMada)
                m = simpleprefix.match(word)
                if m:
                    s1 = getPYAsolutions(htkfword, m.group("prefix")+"<"+m.group("rest"), phondict, useMada)
                    s2 = getPYAsolutions(htkfword, m.group("prefix")+">"+m.group("rest"), phondict, useMada)
            solution = solution or s1 or s2
            if solution:
                pyadict[htkfword] = phondict[htkfword]
            """
            Here if either we didn't ask SAMA for a solution or it didn't find one
            """
            if not solution:
                solution = " ".join(htkfword)
                if not solution in phondict[htkfword]:
                    phondict[htkfword].append(solution)
    phondict['!ENTER'] = ['sil']
    phondict['!EXIT'] = ['sil']
    return phondict, pyadict

def rawMada2Diacritics(l):
    return [(" ").join(w.diacritics for w in s) for s in l]

def containsDash(s):
    return "-" in " ".join(s.split()[2:-1])

def findDashes(rawmada):
    for s in rawMada2Diacritics(rawmada):
        if containsDash(s):
            print 'playPrompt("%s")'%(s)

def playPrompt(s, d="TEMP/wav"):
    try:
        f = re.compile(".*(?P<fname>test-\S*).*").match(s).group("fname")
    except:
        print "'%s' doesn't start with the name of a prompt"%(s)
        return
    segments.sounds.play(os.path.join(d, "%s.wav"%(f)))
    
def praatPrompt(s, d="TEMP/wav"):
    try:
        f = re.compile(".*(?P<fname>test-\S*).*").match(s).group("fname")
    except:
        print "'%s' doesn't start with the name of a prompt"%(s)
        return
    execute("Praat --open %s"%(os.path.join(d, "%s.wav"%(f))))
    
def mada2prompts(src="TEMP", dest="EXPT", promptsfile="originalprompts.segments.mada", useBW=False, N=sys.maxint, pattern=False, grain=1):
    if pattern and isinstance(pattern, str):
        pattern = re.compile(pattern)
    prompts = readRawMada(os.path.join(src, promptsfile), pattern=pattern, N=N, grain=grain)[1]
    for sentence in prompts:
        for w in sentence[2:-1]:
            w.form = HTKFriendly(buck.uni2buck(w.form))
            try:
                w.form.decode("ascii")
            except Exception as e:
                print "SENTENCE %s"%(sentence)
                print "WORD %s"%(w)
                raise(e)
    return prompts
            
hawPrefix = re.compile("(?P<prefix>((f|w) a )?((k a|b i) )?(A l )?).+")
def hamzalwasl(s0):
    if s0 == "A l":
        return s0
    m = hawPrefix.match(s0)
    if m and len(m.group("prefix")) > 0:
        return m.group("prefix")+hamzalwasl(s0[len(m.group("prefix")):])
    m = re.compile("A (i|a|u).*").match(s0)
    if m:
        s1 = s0[2:]
        return s1
    if s0.startswith("A "):
        s1 = "a "+s0[2:]
        return s1
    return s0

hamzas = {re.compile("Y"):"A", re.compile("I|O|{|W"):"Q"}

SVPATTERN = re.compile("(?P<sv>(i y|u w)) (?P<c>[^iauAyw])")
def fixsv(s):
    return SVPATTERN.sub(lambda m: "%s%s %s"%(m.group("sv")[0], m.group("sv")[0], m.group("c")), s)

shaddaPattern = re.compile("(?P<c>.) ~")
FIXSV = 1
FIXHAMZALWASL = 2
FIXCASEMARKERS = 4
FIXSHADDA = 8
FIXSUKUNS = 16
FIXHAMZAS = 32
FIXALL = FIXSV+FIXHAMZALWASL+FIXCASEMARKERS+FIXSHADDA+FIXSUKUNS+FIXHAMZAS
def fixPhonTrans(text0, useMada):
    if useMada & FIXHAMZAS:
        for c in hamzas:
            text0 = c.sub(hamzas[c], text0)
    if useMada & FIXSUKUNS:
        text0 = re.compile("\s*o").sub("", text0)
    """
    delete case markers: this seems to be a GOOD thing
    """
    if useMada & FIXCASEMARKERS:
        if re.compile(".* (i|u|a|F|K|N)$").match(text0):
            text0 = text0[:-2]
    """
    delete shadda/make geminated version of consonant
    """
    if useMada & FIXSHADDA:
        text0 = shaddaPattern.sub("\g<c>", text0)
    """
    replace c0 i y c1 by c0 II c1: might interact weirdly with 
    what we do with shadda if actually we just delete the shadda
    (think it's OK, but be careful)
    """
    if useMada & FIXSV:
        text0 = fixsv(text0)
    """
    Delete initial A unless it's part of Al
    (could have replaced it by Q)
    """
    if useMada & FIXHAMZALWASL:
        text0 = hamzalwasl(text0)
    """
    This will need to be unfixed when we come to apply the assimilation rules
    """
    # text0 = text0.replace("p", "h")
    return text0
    
def makeMadaDict(src, dest, prompts, useMada=3, useBW=False, sep="\n", N=sys.maxint, savePhones=True):
    phondict0 = {}
    phondict1 = {}
    allphones = set()
    """ 
    Read it as UTF-8, split it into SENTENCEs
    """
    phones0 = "#!MLF!#\n"
    phones1 = "#!MLF!#\n"
    for sentence in prompts:
        phones0 += '"%s.lab"\nsil'%(sentence[0][FORM])
        phones1 += '"%s.lab"\nsil'%(sentence[0][FORM])
        for w in sentence[2:-1]:
            form = HTKFriendly(str(buck.uni2buck(w[FORM])))
            phones = " ".join(str(HTKFriendly(w[DIACRITICS])))
            phones = fixPhonTrans(phones, useMada)
            for p in phones.split(" "):
                allphones.add(p)
            incTableN([form, phones], phondict0)
            phones0 += "%s%s"%(sep, phones.replace(" ", sep))
            phones1 += "%s%s%ssp"%(sep, phones.replace(" ", sep), sep)
        if phones1.endswith("%ssp"%(sep)):
            phones1 = phones1[:-3]
        phones0 += "%ssil%s.\n"%(sep, sep)
        phones1 += "%ssil%s.\n"%(sep, sep)
    allphones.add("sil")
    if savePhones:
        with safeout(os.path.join(dest, "phones0.mlf")) as write:
            write(phones0)
        with safeout(os.path.join(dest, "monophones0")) as write:
            for p in sorted(allphones):
                write("%s\n"%(p))
        with safeout(os.path.join(dest, "phones1.mlf")) as write:
            write(phones1)
        allphones.add("sp")
        with safeout(os.path.join(dest, "monophones1")) as write:
            for p in sorted(allphones):
                write("%s\n"%(p))
    phondict0['!ENTER'] = ['sil']
    phondict0['!EXIT'] = ['sil']
    multiEntries = 0
    for form in phondict0:
        if len(phondict0[form]) > 1:
            multiEntries += 1
    if multiEntries > 0:
        print "%s out of %s words have multiple entries"%(multiEntries, len(phondict0))
    return phondict0, {}
                      
def getunrecog(ifile):
    u = set()
    for i in re.compile("\*{3}(?P<u>\S*)\*{3}").finditer(open(ifile).read()):
        u.add(i.group("u"))
    return u

def genAlternateTranscriptions(s, p={"y":["ii"], "w":["uu"]}, alts=0):
    if s == "":
        yield s
    else:
        c = s[0]
        for t in genAlternateTranscriptions(s[1:], p=p, alts=alts):
            yield c+t
            if alts > 0 and c in p:
                for x in p[c]:
                    yield x+t

def removeShortVowels(ifile):
    svPattern = re.compile("(a|i|u)+\s")
    s = ""
    for l in open(ifile):
        if "*/" in l or "!E" in l or "sil" in l:
            s += l
        else:
            s += svPattern.sub("", l)
    print s[:50]
    with safeout(ifile) as write:
        write(s)
            
def getSimplePromptsFromMADAMIRA(prompts0):
    prompts1 = []
    for p in prompts0:
        prompts1.append([buck.uni2buck(w.form) for w in p])
    return prompts1

def fixflags(d):
    return ", ".join("%s:%s"%(k, d[k]) for k in sorted(d.keys()))

def splitTraining(training, S=10000):
    print "splitTraining %s S=%s"%(training, S)
    t = open(training).read().split("\n")
    if len(t) > S:
        print "Splitting training into subsection to allow us to handle large training sets"
    i = 0
    while len(t) > 0:
        i += 1
        with safeout("%s%s.scp"%(training.split(".")[0], i)) as write:
            write("\n".join(t[:S]))
        t = t[S:]

PROMPTNAME = re.compile("(.*/)?(?P<promptname>\S*)")
def filterPrompts(originalprompts, srcwavfiles, srcwavpath, destwavpath):
    filteredprompts = []
    for prompt in originalprompts:
        try:
            wavfile = "%s.wav"%(PROMPTNAME.match(prompt[0].form).group("promptname"))
        except:
            print "weird prompt name? %s"%(prompt[0].form)
        if wavfile in srcwavfiles:
            filteredprompts.append(prompt)
            if not os.path.exists(os.path.join(destwavpath, wavfile)):
                os.link(os.path.join(srcwavpath, wavfile), os.path.join(destwavpath, wavfile))
    return filteredprompts

def checkLength(prompts, destwavpath):
    t = 0.0
    for prompt in prompts:
        t += wavlength(os.path.join(destwavpath, prompt))
    return t

def savePromptsAsTXT(dest, prompts, text):
    with safeout(os.path.join(dest, text)) as write:
        for prompt in prompts:
            write(" ".join(w[FORM] for w in prompt)+"\n")
            
def dataprep(src, dest, expt=False, prompts="originalprompts.txt", makeGrammar=makeBigramGrammar, useMada="madamira3", useProlog=False, useFixedDict='multi', wav="wav", N=sys.maxsize, useconfig=useconfig25, multiStage="yes", testpattern=False, trainingpattern=False, savetestset=True):
    useconfig()
    if not dest:
        dest = os.path.join(src, "EXPT-%s-%s"%(prompts, datetime.now().strftime("%b-%d@%H:%M")))
    makedirs(dest)
    with safeout(os.path.join(dest, "mfcconfig.txt")) as write:
         write(MFCCONFIG)
    with safeout(os.path.join(dest, "config.txt")) as write:
        write(CONFIG)
    madafiles = segments.readMadaFiles(os.path.join(src, "madafiles.json"))
    """
    Read the test prompts, and then read the training ones making sure that you don't use a source file
    that you used for getting the test prompts
    """
    destwavpath = os.path.join(dest, wav)
    """
    Check that every prompt is indeed a prompt and that there is a wav file for it
    """
    srcwavpath = os.path.join(src, wav)
    srcwavfiles = os.listdir(srcwavpath)
    random.seed(0)
    random.shuffle(srcwavfiles)
    """
    Get a fixed random set of test files
    """
    testpattern = re.compile(testpattern)
    testprompts = {}
    i = 0
    makedirs(destwavpath)
    for f in srcwavfiles:
        if f in madafiles and testpattern.match(f):
            testprompts[f] = madafiles[f]
            try:
                os.link(os.path.join(srcwavpath, f), os.path.join(destwavpath, f))
            except:
                pass
            i += 1
            if i == MAXTEST:
                break
    """
    Now get a consistent random set of training files of the required size
    """
    trainingpattern = re.compile(trainingpattern)
    trainingprompts = {}
    available = set(madafiles.keys()).difference(set(trainingprompts.keys()))
    for f in srcwavfiles:
        if f in available and trainingpattern.match(f):
            trainingprompts[f] = madafiles[f]
            try:
                os.link(os.path.join(srcwavpath, f), os.path.join(destwavpath, f))
            except:
                pass
            i += 1
            if i == N:
                break
    print "length of recordings for testing %.2f"%(checkLength(testprompts, destwavpath)/60.0)
    trainingtime = checkLength(trainingprompts, destwavpath)/60.0
    print "length of recordings for training %.2f"%(trainingtime)
    mfcdir="mfc-%s"%(useconfig.__name__)
    toMFC(dest, mfcdir=mfcdir)
    allprompts = "allprompts"
    training = "training.txt"
    testing = "testing.txt"
    wordsmlf(dest, trainingprompts.values()+testprompts.values())
    print "USEMADA %s"%(useMada)
    madamiraPattern = re.compile("madamira(?P<useMada>\d*)")
    samaPattern = re.compile("sama(?P<useMada>\d*)")
    if useMada.startswith("base"):
        phondict0, pyadict = makeBaselineDict(dest, prompts=filteredprompts, useMada="%s.bwf"%(allprompts), N=N)
        baseprompts2phones(dest, "%s.bwf"%(allprompts), training, phondict0, n=0)
        baseprompts2phones(dest, "%s.bwf"%(allprompts), training, phondict0, n=1)
        phondict1 = phondict0
    else:
        m = samaPattern.match(useMada)
        if m:
            IuseMada = int(m.group("useMada"))
            phondict0, pyadict = makePYADict(src, dest, allprompts, IuseMada, N=N, pattern=pattern)
            baseprompts2phones(dest, "%s.bwf"%(allprompts), training, phondict0, n=0)
            baseprompts2phones(dest, "%s.bwf"%(allprompts), training, phondict0, n=1)
            phondict1 = phondict0
        else:
            IuseMada = int(madamiraPattern.match(useMada).group("useMada"))
            phondict0, pyadict = makeMadaDict(src, dest, testprompts.values()+trainingprompts.values(), useMada=IuseMada, N=len(trainingprompts))
            phondict1 = phondict0
    if useProlog:
        print "useProlog %s"%(useProlog)
        phonprompts = "phonprompts%s.txt"%(useProlog)
        """
        I am getting prompts (well actually lots of them) where the
        Prolog rules fail: to overcome that, I'm making prologprep be
        more careful to check that we do indeed have a text and a
        sampa form, and I'm returning the list of prompts where this
        worked as well as the dictionary
        """
        phondict0 = prologprep(dest, allprompts="%s.bwf"%(allprompts), useFixedDict=useFixedDict, phonprompts=phonprompts, useProlog=useProlog)
        phondict1 = phondict0
        phonprompts2phones(dest, "%s.bwf"%(allprompts), training, phonprompts, n=0)
        phonprompts2phones(dest, "%s.bwf"%(allprompts), training, phonprompts, n=1)
    htkformat_dictionary(phondict0, dest, "dict-%s-%s-%s.txt"%(useMada, N, 0), useFixedDict=useFixedDict)
    htkformat_dictionary(phondict1, dest, "dict-%s-%s-%s.txt"%(useMada, N, 1), D=1, useFixedDict=useFixedDict)
    if multiStage == "yes":
        raise Exception("Not fixed for new format")
        shutil.copy(os.path.join(dest, "phones1.mlf"), os.path.join(dest, "phones2.mlf"))
        shutil.copy(os.path.join(dest, "dict-%s-%s-1.txt"%(useMada)), os.path.join(dest, "dict-%s-2.txt"%(useMada), N))
        shutil.copy(os.path.join(dest, "monophones1"), os.path.join(dest, "monophones2"))
        removeShortVowels(os.path.join(dest, "phones0.mlf"))
        removeShortVowels(os.path.join(dest, "phones1.mlf"))
        removeShortVowels(os.path.join(dest, "dict-%s-%s-0.txt"%(useMada, N)))
        removeShortVowels(os.path.join(dest, "dict-%s-%s-1.txt"%(useMada, N)))
    savePromptsAsTXT(dest, trainingprompts.values(), training)
    savePromptsAsTXT(dest, testprompts.values(), testing)
    prompts2code(dest, training, "%s.scp"%(training[:-4]), mfcdir=mfcdir)
    splitTraining(os.path.join(dest, "%s.scp"%(training[:-4])), S=10000)
    prompts2code(dest, testing, "%s.scp"%(testing[:-4]), mfcdir=mfcdir)
    makeGrammar(dest, testprompts.values(), dfile="dict-%s-%s-0.txt"%(useMada, N))
    tdict0, tpyadict = makeMadaDict(src, dest, testprompts.values(), useMada=IuseMada, N=len(trainingprompts), savePhones=False)
    tdict1 = tdict0
    htkformat_dictionary(tdict0, dest, "testdict-0.txt", useFixedDict=useFixedDict)
    htkformat_dictionary(tdict1, dest, "testdict-1.txt", D=1, useFixedDict=useFixedDict)
    if expt:
        expt.testing = testprompts
        expt.training = trainingprompts
        expt.phondict = phondict0
        expt.trainingtime = trainingtime

####################################################################################
# General initialisation stuff: copy wav files & prompts, make .scp and .mlf files #
####################################################################################

def train(d='EXPT', expt=False, training='training.scp', testing='testing.scp', useProlog=False, useMada=False, useForcedAlignment='Y', localtests=50, useTiedList=True, rounds=3, findmismatches=False, phondict=False, quit=100, multiStage="no", useFixedDict="multi", mfcdir="mfc", N=sys.maxint, useconfig=useconfig25):
    T = datetime.now()
    print "TRAINING STARTED %s: useForcedAlignment=%s, useProlog=%s, useMada=%s, multiStage=%s"%(T, useForcedAlignment, useProlog, useMada, multiStage)
    results = []
    """
    We are going to make flat start HMMs for all our phonemes

    If at this point we told it that "all our phonemes" included the
    short vowels, even if dict0.txt and dict1.txt didn't actually
    mention them, then they'd be there when we wanted them later on. Of
    course, they won't get trained till later on, but that's OK.
    """
    hmm0(d, training)
    createHMM0Defs(d)
    print "TRAINING WITH MONOPHONES0 (i.e. without short pauses): %s rounds"%(rounds)
    testdict = "testdict-0.txt"
    for i in range(1, 1+rounds):
        lastdict = "dict-%s-%s-0.txt"%(useMada, N)
        if HERest(d, i, 0):
            test(d, hmm="hmm%s"%(i), hmmlist="monophones0", lexicon=testdict, N=localtests)
            if i > quit:
                return {}
    i += 1
    addShortPauses(d, i)
    i += 2
    testdict = "testdict-1.txt"
    print "TRAINING WITH MONOPHONES1 (i.e. with short pauses this time): %s rounds"%(rounds)
    for i in range(i, i+rounds+(0 if multiStage=="yes" else rounds+2)): # range(6, 8)
        lastdict = "dict-%s-%s-1.txt"%(useMada, N)
        if HERest(d, i, 1):
            test(d, hmm="hmm%s"%(i), hmmlist="monophones1", lexicon=testdict, N=localtests)
            if findmismatches:
                print "MISMATCHES @ %s"%(i)
                findMismatches(d, recout="recouthmm%s.mlf"%(i))
            if i > quit:
                return {}
    if multiStage == "yes":
        i += 1
        print "TRAINING WITH MONOPHONES2 (with short vowels reintroduced): %s rounds"%(rounds)
        lastdict = "dict-%s-%s-2.txt"%(useMada, N)
        for i in range(i, i+rounds+2): # range(6, 8)
            if HERest(d, i, 2):
                test(d, hmm="hmm%s"%(i), hmmlist="monophones2", lexicon=testdict, N=localtests)
                if findmismatches:
                    print "MISMATCHES @ %s"%(i)
                    findMismatches(d, recout="recouthmm%s.mlf"%(i))
                if i > quit:
                    return {}
    testpattern = re.compile('"*/(?P<test>.*).lab"')
    if useForcedAlignment == 'Y':
        print "DO FORCED ALIGNMENT @ %s"%(i)
        aligned(d, i, lexicon=lastdict, mfcdir=mfcdir)
    else:
        print "Skipping realignment -- phones1 was derived by using context sensitive rules, so is supposed to have the right version at each point already"
        shutil.copyfile("%s/phones1.mlf"%(d), "%s/aligned.mlf"%(d))
        shutil.copyfile("%s/training.scp"%(d), "%s/trainingFA.scp"%(d))
        checkDictOK(d, dict=testdict)
    splitTraining(os.path.join(d, "trainingFA.scp"), S=10000)
    i += 1
    print "TRAINING AFTER FORCED ALIGNMENT"
    for i in range(i, i+rounds):
        if HERest(d, i, 1):
            test(d, hmm="hmm%s"%(i), hmmlist="monophones1", lexicon=testdict, N=localtests)
            if i > quit:
                return {}
            if findmismatches:
                print "MISMATCHES @ %s"%(i)
                findMismatches(d, recout="recouthmm%s.mlf"%(i))
    print "PREPARE TRIPHONES @ %s"%(i)
    i += 1
    try:
        prepareTriphones(d, i)
    except Exception as e:
        print "prepareTriphones failed -- this can happen if there's not enough training data"
        raise e
    i += 1
    print "ABOUT TO TRY WITH TRIPHONES @ %s"%(i)
    if HERestBase(d, 'wintri', "trainingFA.scp", i, 'triphones1'):
        try:
            test(d, hmm="hmm%s"%(i), hmmlist="triphones1", lexicon=testdict, N=localtests)
            if findmismatches:
                print "MISMATCHES @ %s"%(i)
                findMismatches(d, recout="recouthmm%s.mlf"%(i))
        except:
            print "Couldn't do test@%s, probably because there are triphones missing from the training data"%(i)
    #Note that 'otherstuff' MUST start with a space
    i += 1
    if HERestBase(d, 'wintri', "trainingFA.scp", i, 'triphones1', otherstuff=' -s %s/stats'%(d)):
        try:
            test(d, hmm="hmm%s"%(i), hmmlist="triphones1", lexicon=testdict, N=localtests)
            if findmismatches:
                print "MISMATCHES @ %s"%(i)
                findMismatches(d, recout="recouthmm%s.mlf"%(i))
        except:
            print "Couldn't do test@%s, probably because there are triphones missing from the training data"%(i)
    if useTiedList:
        i += 1
        print "MAKETIEDLIST @ %s"%(i)
        try:
            makeTiedList(d, i, dict=lastdict)
        except Exception as e:
            print "makeTiedList failed -- this can happen if there's not enough training data"
            raise e
        i += 1
        for i in range(i, i+rounds):
            if HERestBase(d, 'wintri', "trainingFA.scp", i, 'tiedlist'):
                test(d, hmm="hmm%s"%(i), hmmlist="tiedlist", lexicon=testdict, N=localtests)
        if findmismatches:
            print "MISMATCHES @ %s"%(i)
            findMismatches(d, recout="recouthmm%s.mlf"%(i))
        hmm = 'hmm%s'%(i)
        hmmlist = 'tiedlist'
    else:
        hmm = 'hmm%s'%(i)
        hmmlist = 'triphones1'
    T1 = datetime.now()
    flags = {"useProlog":useProlog, "useMada":useMada, "useFixedDict":useFixedDict, "multiStage":multiStage, "useForcedAlignment":useForcedAlignment, "N":N, "useconfig":useconfig.__name__}
    print "TRAINING WITH %s TOOK %s SECONDS\n"%(flags, int((T1-T).total_seconds()))
    if quit == 99:
        return {}
    alignments = test(d, hmm=hmm, hmmlist=hmmlist, lexicon=testdict, name="-%s-%s"%(useMada, useProlog))
    if expt:
        expt.alignments = alignments
    T2 = datetime.now()
    print "#### FINAL TESTING WITH %s TOOK %s ####################################"%(flags, int((T2-T1).total_seconds()))

def checkDictOK(d, aligned="aligned.mlf", dict="dict0.txt"):
     print """
checkDictOK: it can happen that there are phones in dict.txt that don't appear
anyhere in aligned.mlf (this happens if you are using small training sets). If this
happens, you have to remove those items from the dictionary, because forced
alignment falls over if there are phones which don't have HMMs.
"""
     phones = {}
     for line in open("%s/%s"%(d, aligned)):
          if not "test" in line:
               phones[line.strip()] = True
     words = ""
     entries = ""
     for line in open("%s/%s"%(d, dict)):
          line = line.strip()
          for x in space.split(line)[1:]:
               if not x in phones:
                    print "Eliminating %s from dict because %s is not in phones"%(line, x)
                    break
          else:
               entries += line+"\n"
     with safeout("%s/%s"%(d, dict)) as out:
          out(entries)

def findMismatches(dest, words="testing.txt", recout="recout.mlf"):
    words = re.compile("""(?P<test>test\S*)\s*(?P<text>.*)\s*""").finditer(open(os.path.join(dest, words)).read())
    words = [(word.group("test"), word.group("text").split(" ")) for word in words]
    recognised = re.compile("""/(?P<test>[^/]*?).rec"\s*(?P<text>.*?)
\.""", re.DOTALL).finditer(open(os.path.join(dest, recout)).read())
    recognised = [(rec.group("test"), [x.split(" ")[2] for x in rec.group("text").split("\n")]) for rec in recognised]
    swaps = {}
    for w, r in zip(words, recognised):
        """
        w and r both start !ENTER and end !EXIT; so since alignment is N**2 in the
        length of w and r, skipping those two will save time
        """
        for x in dtw.array(w[1][1:-1], r[1][1:-1]).showAlignment():
            if len(x) > 2:
                if not x[0] in swaps:
                    swaps[x[0]] = {}
                try:
                    swaps[x[0]][x[1]].append(w[0])
                except:
                    swaps[x[0]][x[1]] = [w[0]]
    if len(swaps) > 20:
        g = float(len(swaps))/20.0
    else:
        g = 1
    keys = sorted(swaps.keys())
    for i in range(20):
        j = int(i*g)
        if j < len(keys):
            x = keys[j]
            print x
            for y in swaps[x]:
                print "  %s, %s"%(y, swaps[x][y])

def cleanWords(l, d=set(["!ENTER", "!EXIT", "SIL"])):
    return [x for x in l if not x in d]

def splitWords(l):
    return [x.split()[2] for x in l.split("\n")]

def rereadPhonemesAsWords(phonemes, pyadict=False):
    prompts = []
    if pyadict:
        idict = {}
        for x in pyadict:
            for y in pyadict[x]:
                idict[y] = x
    for i in re.compile('"(?P<ID>\S*)"(?P<phonemes>[^\.]*)', re.DOTALL).finditer((re.compile("\d* \d* sil\s*", re.DOTALL).sub("", open(phonemes).read())).strip()):
        words = [" ".join([p.strip().split()[-1] for p in w.strip().split("\n") if not p.strip() == ""]) for w in re.compile("\d* \d* sp\s*", re.DOTALL).split(i.group("phonemes").strip()) if not w.strip() == ""]
        if pyadict:
            words = [idict[w] if w in idict else w.replace(" ", "") for w in words]
        prompts.append((i.group("ID"), words))
    return prompts

def showAlignment(d, recoutlines, testing="testing.txt", recout="recout.mlf", name=""):
    test = [cleanWords(x.strip().split()) for x in open(os.path.join(d, testing))]
    alignments = []
    with safeout(os.path.join(d, "ALIGNED-%s%s"%(recout, name))) as write:
        for t, r in zip(test, recoutlines):
            alignment = dtw.array(t[1:], r[1]).showAlignment()
            alignments.append(alignment)
            write("**** %s, %s ****\n"%(t[0], r[0]))
            for x in alignment:
                if len(x) == 2:
                    write("#### %s ####\n"%(x,))
                else:
                    write("%s\n"%(x,))
    return alignments
    
def test(d, hmm='hmm15', hmmlist='tiedlist', lexicon="dict-tri", testing="testing.txt", o="", N=sys.maxsize, name=""):
    if N == 0:
        return
    hmmrecout = "recout%s.mlf"%(hmm)
    print recout(d, hmm=hmm, hmmlist=hmmlist, lexicon=lexicon, N=N, recout=hmmrecout, o=o)
    recoutlines = [(i.group("TEST"), cleanWords(splitWords(i.group("WORDS").strip()))) for i in re.compile('"(?P<TEST>\S*)"(?P<WORDS>.*?)\n\.', re.DOTALL).finditer(open(os.path.join(d, hmmrecout)).read())]
    return showAlignment(d, recoutlines, testing=testing, recout=hmmrecout, name=name)
    
def readAlignments(d, alignment):
    return [i.group("word") for i in re.compile("#### \('(?P<word>\S*)', '\S*'\) ####").finditer(open(os.path.join(d, alignment)).read())]

def recout(d, hmm, hmmlist, lexicon, config="config.txt", N=10000, recout="recout.mlf", o=" -o SWT"):
    with safeout(os.path.join(d, "temp.scp")) as write:
        write("\n".join(readPrompts(os.path.join(d, "testing.scp"))[:N]))
    execute('HVite -A -D -T %s -p 2.0 -s 10.0 -H %s/%s/macros -H %s/%s/hmmdefs -C %s/%s -S %s/temp.scp -i %s/%s -w %s/wdnet %s %s/%s %s/%s'%(DEBUG, d, hmm, d, hmm, d, config, d, d, recout, d, o, d, lexicon, d, hmmlist))
    r = execute('HResults -e ??? !ENTER -e ??? !EXIT -e ??? SIL -I %s/words.mlf %s/%s %s/%s'%(d, d, hmmlist, d, recout))[0]
    return r
     
def createHMM0Defs(d, hmm0='hmm0', monophones='monophones0', hmm0defs='hmmdefs', hmmdefs='hmmdefs', macros='macros'):
    hmm0 = os.path.join(d, hmm0)
    proto = open(os.path.join(hmm0, "proto")).read()
    hmms = re.compile('.*(?P<proto><BEGINHMM>.*<ENDHMM>).*', re.DOTALL).match(proto).group('proto')
    with safeout(os.path.join(hmm0, hmmdefs)) as write:
        for phone in open(os.path.join(d,monophones)):
            write('~h "%s"\n%s\n'%(phone.strip(), hmms))
    with safeout('%s/%s'%(hmm0, macros)) as write:
        hmm0macros = re.compile('(?P<header>.*<DIAGC>).*', re.DOTALL).match(proto).group('header')
        hmm0macros = hmm0macros+'\n'+open('%s/%s'%(hmm0, 'vFloors'), 'r').read()
        write(hmm0macros)

def toMFC(dest, wav="wav", mfcdir="mfc"):
    wavdir = os.path.join(dest, wav)
    mfcdir = os.path.join(dest, mfcdir)
    wavfiles = os.listdir(wavdir)
    makedirs(mfcdir)
    mfcfiles = os.listdir(mfcdir)
    print "toMFC(%s, %s): already contains %s files"%(wavdir, mfcdir, len(mfcfiles))
    if len(wavfiles) == len(mfcfiles):
        print "%s already populated"%(mfcdir)
    else:
        with safeout(os.path.join(dest, "mfc.scp")) as write:
            for wavfile in wavfiles:
                if not "%s/%s.mfc"%(mfcdir, wavfile[:-4]) in mfcfiles:
                    write("%s/%s %s/%s.mfc\n"%(wavdir, wavfile, mfcdir, wavfile[:-4]))
        # print "%s/%s contains %.2f minutes of recorded material"%(dest, wav, wavlength(os.path.join(dest, wav))/60.0)
        codetrain(dest, "mfc.scp")
                      
def codetrain(d, scp, useconfig=useconfig25):
     makedirs(os.path.join(d, "mfc-%s"%(useconfig.__name__)))
     execute("HCopy -A -D -T %s -C %s/mfcconfig.txt -S %s"%(DEBUG, d, os.path.join(d, scp)))

def hmm0(d, train):
    makedirs(os.path.join(d, 'hmm0'))
    with safeout(os.path.join(d, "proto.txt")) as write:
        write(PROTO)
    execute('HCompV -A -D -T %s -C %s/config.txt -f 0.01 -m -S %s/%s -M %s/hmm0 %s/proto.txt'%(DEBUG, d, d, train, d, d))
    
def HERest(d, n, k):
     return HERestBase(d, 'phones%s'%(k), "training.scp", n, 'monophones%s'%(k))

def HERestBase(d, mlf, train, n, monophones, otherstuff=''):
    hmmdir = os.path.join(d, "hmm%s"%(n))
    makedirs(hmmdir)
    print "Calling HERest in %s"%(os.getcwd())
    HEREST = 'HERest -A -D -T %s -C %s/config.txt -I %s/%s.mlf -t 250.0 150.0 1000.0 -S %s/%s -H %s/hmm%s/macros -H %s/hmm%s/hmmdefs -M %s/hmm%s%s %s/%s'%(DEBUG, d, d, mlf, d, train, d, n-1, d, n-1, d, n, otherstuff, d, monophones)
    return execute(HEREST)     
   
"""
From http://disfruta555.blogspot.co.uk/2015/04/trouble-shooting-htk-error-7031.html

HERest -S trnlist1 -I lab1 -H dir1/hmmdefs1 -M dir2 -p 1 hmmlist    # part1
HERest -S trnlist2 -I lab2 -H dir1/hmmdefs2 -M dir2 -p 2 hmmlist    # part2
HERest                     -H dir1/hmmdefs  -M dir2 -p 0 hmmlist  dir2/*.acc # Merging

You have to copy the macros as well as the hmmdefs from the dir1

For some weird reason I have to list all the .acc files when I run it
from inside Python, though it works fine with *.acc from the command
line.

"""
      
def HERestBase(d, mlf, train, n, monophones, otherstuff=''):
    hmmdir = os.path.join(d, "hmm%s"%(n))
    makedirs(hmmdir)
    p = 1
    tPattern = re.compile("%s(?P<i>\d+).scp"%(train.split(".")[0]))
    for t in os.listdir(d):
        m = tPattern.match(t)
        if m:
            i = m.group("i")
            HEREST = 'HERest -A -D -T %s -C %s/config.txt -I %s/%s.mlf -t 250.0 150.0 1000.0 -S %s/%s -H %s/hmm%s/macros -H %s/hmm%s/hmmdefs -M %s/hmm%s%s -p %s %s/%s'%(DEBUG, d, d, mlf, d, t, d, n-1, d, n-1, d, n, otherstuff, i, d, monophones)
            execute(HEREST)
            p += 1
    accs = " ".join([os.path.join(d, "hmm%s"%(n), acc) for acc in os.listdir(os.path.join(d, "hmm%s"%(n))) if acc.endswith(".acc")])
    HEREST = """HERest -C %s/config.txt -H %s/hmm%s/macros -H %s/hmm%s/hmmdefs -M %s/hmm%s%s -p 0 %s/%s %s"""%(d, d, n-1, d, n-1, d, n, otherstuff, d, monophones, accs)
    return execute(HEREST)

def addShortPauses(d, i=4):
    hmm4 = os.path.join(d, 'hmm%s'%(i))
    makedirs(hmm4)
    hmmdefs3 = open(os.path.join(d, "hmm%s"%(i-1), "hmmdefs")).read()
    silModel = re.compile('.*~h "sil".*?<STATE> 3(?P<SIL>.*?)<STATE>.*', re.DOTALL).match(hmmdefs3).group('SIL')
    spModel = """~h "sp"
<BEGINHMM>
<NUMSTATES> 3
<STATE> 2%s<TRANSP> 3
 0.0 1.0 0.0
 0.0 0.9 0.1
 0.0 0.0 0.0
<ENDHMM>
"""%(silModel)
    with safeout(os.path.join(hmm4, "hmmdefs")) as write:
        write(hmmdefs3+spModel)
    shutil.copy('%s/hmm%s/macros'%(d, i-1), '%s/hmm%s/macros'%(d, i))
    with safeout(os.path.join(d, "sil.hed")) as write:
        write("""
AT 2 4 0.2 {sil.transP}
AT 4 2 0.2 {sil.transP}
AT 1 3 0.3 {sp.transP}
TI silst {sil.state[3],sp.state[2]}
""")
    i += 1
    print "Applying sil.hed to hmm%s"%(i-1)
    makedirs(os.path.join(d, 'hmm%s'%(i)))
    execute('HHEd -A -D -T %s -H %s/hmm%s/macros -H %s/hmm%s/hmmdefs -M %s/hmm%s %s/sil.hed %s/monophones1'%(DEBUG, d, i-1, d, i-1, d, i, d, d))
    print "OK, hmm%s should now be created and populated"%(i)

def aligned(dest, i, lexicon="dict1.txt", mfcdir="mfc"):
    hmm = os.path.join(dest, "hmm%s"%(i))
    print "aligned(%s)"%(hmm)
    testpattern = re.compile('"*/(?P<test>.*).lab"')
    standard = "HVite -A -D -T %s -p 2.0 -s 10.0 -l * -o SWT -a -b !EXIT -m -C %s/config.txt -H %s/macros -H %s/hmmdefs -i %s/aligned.mlf -t 250.0 150.0 1000.0 -y lab -I %s/words.mlf -S %s/trainingFA.scp %s/%s %s/monophones1"%(DEBUG, dest, hmm, hmm, dest, dest, dest, dest, lexicon, dest)
    execute(standard)
    properlyAligned = set('"%s/%s/%s.mfc"'%(dest, mfcdir, i.group("file")) for i in re.compile("(?P<file>test\S+)\.").finditer(open("%s/aligned.mlf"%(dest)).read()))
    """
    After this we are going to use
    
        training.scp
        triphones1
        wintri.mlf
        monophones1
        dict1.txt
        tiedlist

    training.scp can certainly be a mess, because it may mention sentences that we didn't align
    It would be nice if fixing that fixed everything. 
    """
    training = open(os.path.join(dest, "training.scp")).read().strip().split("\n")
    notaligned = []
    with safeout(os.path.join(dest, "training1.scp")) as write:
        for prompt in training:
            if prompt in properlyAligned:
                write("%s\n"%(prompt))
            else:
                notaligned.append(prompt)
    print "%s out of %s were not successfully realigned"%(len(notaligned), len(training))
    for n in notaligned[:10]:
        print "* %s"%(n)
    return properlyAligned

def prepareTriphones(d, i=10, monophones="monophones1", trihed="mktri.hed"):
    # standard version
    mktri = """
WB sp
WB sil
TC
"""
    # but I want to try one where sp takes part properly in triphones
    mktri1 = """
WB sil
TC
"""
    with safeout(os.path.join(d, "mktri.led")) as write:
        write(mktri)
    execute('HLEd -A -D -T %s -n %s/triphones1 -l * -i %s/wintri.mlf %s/mktri.led %s/aligned.mlf'%(DEBUG, d, d, d, d))
    with safeout("%s/%s"%(d, trihed)) as write:
        write("CL %s/triphones1\n"%(d))
        for phone in open("%s/%s"%(d, monophones)):
            phone = phone.strip()
            write("TI T_%s {(*-%s+*,%s+*,*-%s).transP}\n"%(phone, phone, phone, phone))
    hmm0 = os.path.join(d, "hmm%s"%(i-1))
    hmm1 = os.path.join(d, "hmm%s"%(i))
    # triphones(d)
    makedirs(hmm1)
    execute('HHEd -A -D -T %s -H %s/macros -H %s/hmmdefs -M %s %s/mktri.hed %s/monophones1'%(DEBUG, hmm0, hmm0, hmm1, d, d))
     
def monophones2TB(t, monophones):
     mphones = readPrompts(monophones)
     TB = ''
     for i in range(2, 5):
          for m in mphones:
               TB = TB+'TB %s "ST_%s_%s_" {("%s","*-%s+*","%s+*","*-%s").state[%i]}\n'%(t, m, i, m, m, m, m, i)
     return TB

def makeTiedList(d, i, dict="dict1.txt"):
    execute('HDMan -A -D -T %s -b sp -n %s/fulllist -g %s/global.ded -l %s/flog %s/dict-tri %s/%s'%(DEBUG, d, d, d, d, d, dict))
    allphones = {}
    for phone in open('%s/fulllist'%(d)).readlines()+open('%s/triphones1'%(d)).readlines():
        allphones[phone] = True
    with safeout('%s/fulllist'%(d)) as write:
        for phone in allphones:
            write(phone)
    with safeout('%s/tree.hed'%(d)) as write:
        write("""
RO 100 %s/stats
TR 2
"""%(d))
        TB = monophones2TB(350, '%s/monophones1'%(d))
        write(TB)
        write("""
TR 1
 
AU "%s/fulllist"
CO "%s/tiedlist" 
 
ST "%s/trees" 
"""%(d, d, d))
    hmm0 = os.path.join(d, "hmm%s"%(i-1))
    hmm1 = os.path.join(d, "hmm%s"%(i))
    makedirs(hmm1)
    execute('HHEd -A -D -T %s -H %s/macros -H %s/hmmdefs -M %s %s/tree.hed %s/triphones1'%(DEBUG, hmm0, hmm0, hmm1, d, d))

##############################################################
#                 top-level scripts                          #
##############################################################

def extractResults(rfile="results.txt", out=sys.stdout):
    if not "\n" in rfile:
        rfile = open(rfile).read()
    expts = {}
    namePattern = re.compile("(?P<name>.*?)#+.*", re.DOTALL)
    resultsPattern = re.compile("Corr=(?P<correct>\S*?),")
    for expt in rfile.split("#### EXPERIMENT"):
        name = namePattern.match(expt)
        if name:
            name = name.group("name").strip()
            shortname = re.compile(" |:|,").sub("", re.compile("[a-zA-Z]+:\s*(?P<AZ>[a-zA-Z0-9]+)").sub(lambda m: m.group("AZ")[0], name))
            print "%s -> %s"%(name, shortname)
            expts[shortname] = []
            for i in resultsPattern.finditer(expt):
                expts[shortname].append(i.group("correct"))
    return expts

def enlist(s):
    if isinstance(s, list):
        return s
    else:
        return [s]
    
def experiment(src="SRC", dest="TEST", prompts="", dicts=["multi", "fixed"], prolog=0, stages="no", mada="madamira%s"%(0), forcedAlignment=["Y", "N"], configs=useconfig39, N=sys.maxsize, localtests=10, quit=100, testpattern=False, trainingpattern=False, savetestset=True):
    makedirs(dest)
    makeGrammar = makeGrammar1
    makeGrammar = makeBigramGrammar
    prolog = enlist(prolog)
    dicts = enlist(dicts)
    mada = enlist(mada)
    stages = enlist(stages)
    forcedAlignment = enlist(forcedAlignment)
    configs = enlist(configs)
    N = enlist(N)
    experiments = []
    for useconfig in configs:
        clean(dest)
        for useFixedDict in dicts: 
            for useForcedAlignment in forcedAlignment:
                if useFixedDict == "fixed" and useForcedAlignment == "Y":
                    print "Skipping useForcedAlignment=%s, useFixedDict=%s"%(useForcedAlignment, useFixedDict)
                    continue
                for useMada in mada: 
                    if useMada.startswith("base") and useFixedDict=="multi":
                        print "Skipping useMada=%s, useFixedDict=%s"%(useMada, useFixedDict)
                        continue
                    for useProlog in prolog:
                        if useProlog > 0 and (useMada.startswith("sama") or useMada.startswith("base")):
                            print "Skipping useMada=%s, useProlog=%s"%(useMada, useProlog)
                            continue
                        for multiStage in stages:
                            if multiStage == "yes" and not useMada.startswith("madamira"):
                                print "Skipping useMada=%s, multiStage=%s"%(useMada, multiStage)
                                continue
                            for n in N:
                                print "#### EXPERIMENT %s ########"%(fixflags({"useProlog":useProlog, "useMada":useMada, "useFixedDict":useFixedDict, "multiStage":multiStage, "useForcedAlignment":useForcedAlignment, "N":n, "config":useconfig.__name__}))
                                experiments.append(EXPERIMENT(src, dest, prompts, useMada, useProlog, useFixedDict, useconfig, makeGrammar, useForcedAlignment, localtests, multiStage=multiStage, N=n, quit=quit, testpattern=testpattern, trainingpattern=trainingpattern, savetestset=savetestset))
    return experiments

class EXPERIMENT:

    def __init__(self, src, dest, prompts, useMada, useProlog, useFixedDict, useconfig, makeGrammar, useForcedAlignment, localtests, N=sys.maxsize, quit=100, multiStage="no", testpattern=False, trainingpattern=False, savetestset=True):
        self.src = src
        self.dest = dest
        self.prompts = prompts
        self.useProlog = useProlog
        self.useMada = useMada
        self.useFixedDict = useFixedDict
        self.useconfig = useconfig
        self.makeGrammar = makeGrammar
        self.N = N
        self.localtests = localtests
        self.useForcedAlignment = useForcedAlignment
        self.multiStage = multiStage
        """
        Absolutely bog-standard dataprep
        """
        dataprep(src, dest, expt=self, prompts=prompts, useProlog=useProlog, useMada=useMada, useFixedDict=useFixedDict, useconfig=useconfig, makeGrammar=makeGrammar, N=N, multiStage=self.multiStage, testpattern=testpattern, trainingpattern=trainingpattern, savetestset=savetestset)
        """
        Then do training
        """
        train(dest, expt=self, useProlog=useProlog, useMada=useMada, useForcedAlignment=useForcedAlignment, localtests=localtests, phondict=self.phondict, quit=quit, multiStage=self.multiStage, mfcdir="mfc-%s"%(useconfig.__name__), N=N, useconfig=useconfig, useFixedDict=useFixedDict) 

    def score(self):
        i = 0
        d = 0
        x = 0
        n = 0
        c = 0
        for a in self.alignments:
            for w in a:
                if w[0] == "*":
                    i += 1
                elif w[1] == "*":
                    d += 1
                    n += 1
                elif len(w) == 3:
                    x += 1
                    n += 1
                else:
                    c += 1
                    n += 1
        return float(c)/float(n), c, i, d, x, n

def expts2csv(expts, out=sys.stdout):
    with safeout(out) as write:
        write("training #\ttraining time\taccuracy\n")
        for e in expts:
            write("%s\t%.2f\t%.2f\n"%(len(e.training), e.trainingtime, e.score()[0]))
        

###################################################################
# bits and pieces for debugging and generally poking around       #
###################################################################

def getHMMs(dest, hmm):
    return sorted([h.group("hmm") for h in re.compile('~h "(?P<hmm>\S*)"').finditer(open(os.path.join(dest, hmm, "hmmdefs")).read())])

def getAllTriphones(dest):
    return sorted(open(os.path.join(dest, "triphones1")).read().strip().split("\n"))

def wintriphones(dest, words="wintri.mlf"):
    phones = set()
    for test in re.compile("""
"\*/test\S*"
(?P<text>.*?)
\.""", re.DOTALL).finditer(open(os.path.join(dest, words)).read()):
        for x in test.group("text").split("\n"):
            phones.add(x)
    return phones

def checkArgs(argTable, args):
    for a in args:
        k0, v = a.split("=")
        for k1 in argTable:
            if k1.startswith(k0):
                argTable[k1] = v
                break
        else:
            raise Exception("Unknown arg on command line: %s"%(a))
    return argTable

def results2csv(rfile="results.txt", out=sys.stdout):
    results = open(rfile).read()
    p0 = re.compile(""".*?TRAINING WITH USEPROLOG=\s*(?P<prolog>\S*)\s*TOOK""", re.DOTALL)
    p1 = re.compile("""WORD: %Corr=(?P<correct>\S*), Acc=\S* \[H=\S*, D=\S*, S=\S*, I=\S*, N=\S*\]""")
    results = [(i0.group("prolog"), [i1.group("correct") for i1 in p1.finditer(i0.group(0))]) for i0 in p0.finditer(results)]
    with safeout(out) as write:
        write("\t".join("F%s"%(h[0]) for h in results)+"\n")
        for i in range(len(results[0][1])):
            write("\t".join(h[1][i] for h in results)+"\n")
    
USEAGE = """HTK.py src=<src> dest=<dest> (prompts=<allprompts.txt> N=<100000> PROLOG=<1> realign=Y/N fixeddict=multi/fixed, useSavedMFC=Y/N)
(unique abbreviated flags are OK, e.g. useF rather than useFixedDict)"""
if "HTK.py" in sys.argv[0]:
    argTable = checkArgs({"src":False, "dest":False, "N":100000, "PROLOG":1, 
                          "realign":"Y", "fixeddict": "multi", "grammar": "bigrammar",
                          "savedMFC": "Y", "prompts": "originalprompts.txt"},
                          sys.argv[1:])
    src = argTable["src"]
    if not src:
        print "You must specify the src directory where the original data is kept"
        print USEAGE
        sys.exit(1)
    dest = argTable["dest"]
    if not dest:
        print "You must specify the dest directory where the experiment is to be carried out"
        print USEAGE
        sys.exit(1)
    N = int(argTable["N"])
    useProlog = int(argTable["PROLOG"])
    useForcedAlignment = argTable["realign"]
    useFixedDict = argTable["fixeddict"]
    grammar = argTable["grammar"]
    useSavedMFC = argTable["savedMFC"],
    prompts = argTable["prompts"]
    doItAll(src, dest, useProlog=useProlog, useForcedAlignment=useForcedAlignment, useFixedDict=useFixedDict, useSavedMFC=useSavedMFC, grammar=grammar, prompts=prompts, N=N)

