# -*- coding: utf-8 -*-
#!/usr/bin/python

"""
expts = experiment(src= "SRC", dest="DEST", dicts="fixed", versions=["MADA:4"], forcedAlignment="N", prolog=4, configs=useconfig25, maintests=100, localtests=0, N=[500], testpattern=".*", trainingpattern=".*")
"""

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
    files = {"data": map(re.compile, ['allprompts.txt', 'monophones.*', 'phones..mlf', 'testing.scp', 'training.txt', 'config.txt', 'mfc.scp', 'phonprompts.*.txt', 'testing.txt', 'mfcconfig.txt', 'prompts.pl', 'training.*.scp', 'words.mlf']),
             "wav": map(re.compile, ["wav", 'mfc*']),
             "genfiles": map(re.compile, ['allprompts.txt', 'monophones.*', 'phones..mlf', 'testing.scp', 'training.txt', 'training.pck', 'config.txt', 'mfc.scp', 'phonprompts.*.txt', 'testing.txt', 'mfcconfig.txt', 'prompts.pl', 'training.scp', 'words.mlf', 'wdnet']),
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
    written = set()
    fixed = set(['!ENTER', '!EXIT', 'silence', 'sil', 'sp'])
    print dict["sp"]
    with safeout(os.path.join(exptdir, dictfile)) as write:
        for key in sorted(dict.iterkeys()):
            if not key in fixed: # to prevent putting their vales ('sil') as phones 
                entries = sorted(dict[key])
                if len(entries) > 1:
                    m += 1
                for value in entries:
                    s = '%s\t%s%s\n'%(key, value, sp)
                    if not s in written:
                        write(s)
                        written.add(s)
                    if useFixedDict == "fixed" or D == 0:
                        break
            else:
                write('%s\t%s\n'%(key, 'sil'))
    if D == 1:
        print "%s words out of %s had multiple entries"%(m, len(dict))
    return m

VERSION = re.compile("(?P<version>MADA|IMAN|SAMA|LDC|RAW):(?P<fixphontrans>\d+)")

##################################################################
#                        Prolog stuff                            #
##################################################################


def genPrologPromptsFromPhones(dest, name, prompts, N=sys.maxint):
    s = "["
    DIACRITICS = 1
    for i, prompt in enumerate(prompts):
        if i == N:
            break
        text = " ".join(word[DIACRITICS].replace(" ", "") for word in prompts[prompt][2:-1])
        if not text == '':
            s += 'sentence("%s", "%s"),\n'%(prompt, text)
    s = s[:-2]+"].\n"
    with safeout(os.path.join(dest, "prompts-%s.pl"%(name))) as write:
        write(s)
    
HARMONY = "HARMONY-09-12-2017"
def runprolog(d, name, phonprompts="phonprompts.txt", useProlog=1):
    prolog = ["sicstus", "-r", "%s/test.sav"%(HARMONY), "-a", d, "prompts-%s.pl"%(name), phonprompts, str(useProlog)]
    print prolog
    print "Running phonological rules--takes a little while, can't print progress messages because we're capturing the output"
    x = execute(prolog)
    print "Done: %s"%(x[1])
        
def noEmptyStrings(l):
    return [x for x in l if not x == '' and not x == ' + ']

prologPrompt = re.compile("""'(?P<prompt>\S*)'
text:(?P<text>[^\n]*)
sampa:(?P<sampa>[^\n]*)
""", re.DOTALL)

def readPrologPrompts(dest, phonprompts):
    print "readPrologPrompts('%s', '%s')"%(dest, phonprompts)
    prompts = {}
    for i in prologPrompt.finditer(open(os.path.join(dest, phonprompts)).read()):
        prompts[i.group("prompt")] = i.group("sampa")
    return prompts
    
def prologprep(exptdir, name, prompts0, useProlog=1, useFixedDict='multi', N=sys.maxint):
    """ Get the original prompts into a Prolog-friendly format """
    genPrologPromptsFromPhones(exptdir, name, prompts0, N=N)
    """
    Run Iman's phonological rules to generate a set of prompts. Once we've done this we have a
    contextually determined set of prompts
    """
    phonprompts = "phonprompts-%s-%s.txt"%(name, useProlog)
    runprolog(exptdir, name, phonprompts=phonprompts, useProlog=useProlog)
    """
    Use the prompts you just generated to create a dictionary. This may be 
    multi-entry dictionary: this will happen if any of the phonological rules
    applied across word boundaries, so (a) you have to have rules that *can*
    apply across word boundaries and (b) there have to have been occurrences
    of words in contexts that differ in such a way as to cause those rules
    to apply. So for the simpler rules or for small training sets it's quite
    likely that the dictionary will only have a single entry per word.
    """
    prompts1 = readPrologPrompts(exptdir, phonprompts)
    removing = """
If we get a mismatch between the number of words in the Madamira
output and the number of words in Iman's transcription, it's tempting
to just use the Madamira transcription, but there are odd cases where
that doesn't work. In particular, in some case Iman changes the names
of consonants; then, if we get an instance of that in a mismatched
example we may end up with a very small number of instances of the
Madamira name for the consonant, which causes problems with
makeTiedList. I am therefore currently just removing those examples
from the data (which is prompts0, not prompts1).
"""
    for k in prompts1:
        prompt0 = prompts0[k]
        prompt1 = (re.compile("\s*\#+\s*$").sub("", prompts1[k])).strip()
        prompt1 = re.compile("\s*\#+\s*").split(prompt1)
        if len(prompt0)-3 == len(prompt1):
            for x, y in zip(prompt0[2:-1], prompt1):
                x[1] = y
        else:
            if removing:
                print removing
                removing = False
            print "Wrong number of words in Prolog version of prompt %s"%(prompt0[0][0])
            print "\nMADA %s"%(" # ".join([x[1] for x in prompt0[2:-1]]))
            print "\nIMAN %s"%(" # ".join([re.compile("\s*").sub("", x) for x in prompt1]))
            del prompts0[k]
            """
            for x in prompt0[2:-1]:
                x[1] = " ".join(HTKFriendly(x[1]))
            """
    removing = """
If the Prolog rules failed there will be an entry in prompts0 (which
is derived from Mada) but not in prompts1 (which is generated by the
Prolog rules), and if that happens then everything will fall apart
"""
    for p in prompts0.keys():
        if not p in prompts1:
            if removing:
                print removing
                removing = False
            print "Removing %s, %s from promptso"%(p, prompts0[p])
            del prompts0[p]
    return prompts0
    
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

def makeBigramGrammar(d, prompts, dfile="dict0.txt", wmlf="words"):
    words = set()
    with safeout(os.path.join(d, "wlist")) as write:
        for prompt in prompts:
            for w in prompt[1:]:
                w = w[FORM]
                if not w in words:
                    write(w+"\n")
                    words.add(w)
    execute("HLStats -b %s/bigfn -o %s/wlist %s/%s.mlf"%(d, d, d, wmlf))
    execute("HBuild -n %s/bigfn %s/wlist %s/wdnet-%s"%(d, d, d, wmlf))

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
            
def wordsmlf(d, prompts, words="words.mlf"):
    with safeout(os.path.join(d, words)) as write:
        write("#!MLF!#\n")
        for prompt in prompts:
            pform = prompt[0][FORM]
            if not pform.startswith("*/"):
                pform = "*/%s"%(pform)
            write(""""%s.lab"
%s
.
"""%(pform, "\n".join(w[FORM] for w in prompt[1:])))

def playPrompt(s, d="TEMP/wav"):
    try:
        f = re.compile(".*(?P<fname>test-\S*).*").match(s).group("fname")
    except:
        print "'%s' doesn't start with the name of a prompt"%(s)
        return
    segments.sounds.play(os.path.join(d, "%s.wav"%(f)))
            
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

hamzas = {re.compile("Y"):"A", re.compile("I|O|{|W|G"):"Q"}

def fixsv(s):
    SVPATTERN = re.compile("(?P<sv>(i y|u w)) (?P<c>[^iauAyw])")
    return SVPATTERN.sub(lambda m: "%s%s %s"%(m.group("sv")[0], m.group("sv")[0], m.group("c")), s)

shaddaPattern = re.compile("(?P<c>.) ~")
FIXSV = 1
FIXHAMZALWASL = 2
FIXCASEMARKERS = 4
FIXSHADDA = 8
FIXSUKUNS = 16
FIXHAMZAS = 32
FIXMOST = FIXCASEMARKERS+FIXSHADDA+FIXSUKUNS+FIXHAMZAS
FIXALL = FIXSV+FIXHAMZALWASL+FIXCASEMARKERS+FIXSHADDA+FIXSUKUNS+FIXHAMZAS
BASE = 64
REMSHADDA = 128

def fixPhonTrans(text0, fixphontrans):
    """
    Just replace LDC's hamza by ours: might as well just always do that -- has no
    significance, but it means that we can use Iman's rules with LDC transcriptions
    """
    text0 = text0.replace("G", "O")
    if fixphontrans & BASE:
        text0 = re.compile("\s(a|i|o|u|~|(F|K|N)$)").sub("", text0)
    if fixphontrans & (FIXHAMZAS | BASE):
        for c in hamzas:
            text0 = c.sub(hamzas[c], text0)
    if fixphontrans & FIXSUKUNS:
        text0 = re.compile("\s*o").sub("", text0)
    """
    delete case markers: this seems to be a GOOD thing
    """
    if fixphontrans & FIXCASEMARKERS:
        if re.compile(".*\s(i|u|a|F( A)?|K|N)$").match(text0):
            text0 = text0[:-2]
    """
    delete shadda/make geminated version of consonant
    """
    s = text0
    if fixphontrans & FIXSHADDA:
        text0 = shaddaPattern.sub("\g<c> \g<c>", text0)
    if fixphontrans & REMSHADDA:
        text0 = text0.replace(" ~", "")
    """
    replace c0 i y c1 by c0 II c1: might interact weirdly with 
    what we do with shadda if actually we just delete the shadda
    (think it's OK, but be careful)
    """
    if fixphontrans & FIXSV:
        text0 = fixsv(text0)
    """
    Delete initial A unless it's part of Al
    (could have replaced it by Q)
    """
    if fixphontrans & FIXHAMZALWASL:
        text0 = hamzalwasl(text0)
    """
    This will need to be unfixed when we come to apply the assimilation rules
    """
    # text0 = text0.replace("p", "h")
    return text0
  

def unique(l0):
    l1 = []
    seen = set()
    for x in l0:
        if not x in seen:
            l1.append(x)
            seen.add(x)
    return l1

def fixRAWPrompts(dest="EXPT", ardictfile="ar-ar_lexicon_2014-03-17.txt", prompts=False, sep="\n", N=sys.maxint, pattern=False, fixphontrans=0, useProlog=0):
    xdict = {"!ENTER":"sil", "!EXIT":"sil", "sp":"sil"}
    for prompt in prompts:
        sentence = prompts[prompt]
        for word in sentence[2:-1]:
            word[FORM] = HTKFriendly(word[FORM])
            form = word[FORM]
            transcription = " ".join(HTKFriendly(form.replace("-", "")))
            xdict[form] = [transcription]
            word[1] = transcription
    return xdict

"""
The prompts start life as old style BW, because that's what we need
for SAMA, and it's easier to convert from old to new than to be
confident that we can convert back from new to old. But the LDC
dictionary needs new style, so we need to do the conversion at the
right points in here. We also need to do HTKFriendly on the
transcriptions that we retrieve, and then we have to get rid of "U" in
the retrieved transcriptions, because it's pretty rare and we don't
believe in it anyway.
"""
def readLDCDict(ldcdictfile="ar-ar_lexicon_2014-03-17.txt", fixphontrans=0):
    ldcdict = {}
    for line in open(ldcdictfile):
        line = line.strip()
        if not (line == "" or line.startswith("#")):
            word, wphones = line.split(" ", 1)
            wphones = HTKFriendly(fixPhonTrans(wphones, fixphontrans))
            word = HTKFriendly(word)
            try:
                ldcdict[word].append(wphones)
            except:
                ldcdict[word] = [wphones]
    return ldcdict

def fixLDCPrompts(dest="EXPT", ldcdictfile="ar-ar_lexicon_2014-03-17.txt", prompts=False, sep="\n", N=sys.maxint, pattern=False, fixphontrans=0, useProlog=0):
    ldcdict = readLDCDict(ldcdictfile=ldcdictfile, fixphontrans=fixphontrans)
    print "LDC contains %s words"%(len(ldcdict))
    known = {}
    unknown = {}
    xdict = {"!ENTER":"sil", "!EXIT":"sil", "sp":"sil"}
    for prompt in prompts:
        sentence = prompts[prompt]
        prompt1 = sentence[:2]
        for word in sentence[2:-1]:
            word[FORM] = HTKFriendly(word[FORM]).replace("-", "")
            form = word[FORM]
            try:
                transcription = ldcdict[form][0]
                incTable(form, known)
                xdict[form] = ldcdict[form]
            except:
                transcription = " ".join(HTKFriendly(form.replace("-", "")))
                incTable(form, unknown)
                try:
                    xdict[form].append(transcription)
                except:
                    xdict[form] = [transcription]
            prompt1.append([word[0], transcription])
        prompt1.append(sentence[-1])
        prompts[prompt] = prompt1
    if useProlog:
        prompts = prologprep(dest, "LDC-%s"%(fixphontrans), prompts, useProlog=useProlog)
        for prompt in prompts:
            sentence0 = prompts[prompt]
            sentence1 = sentence0[:2]
            for w in sentence0[2:-1]:
                xdict[w[0]] = [w[1]]
    else:
        for k in xdict:
            if k in ldcdict:
                for t in ldcdict[k]:
                    if not t in xdict[k]:
                        xdict[k].append(t)
    print "%s distinct words were present in LDC, %s were missing (%.2f); %s known tokens, %s unknown tokens (%.2f))"%(len(known), len(unknown), float(len(unknown))/float(len(known)+len(unknown)), sum(known.values()), sum(unknown.values()), float(sum(unknown.values()))/float(sum(known.values())+sum(unknown.values())))
    return xdict

MFORM = 0
MPHONES = 1
def fixMadaPrompts(dest="EXPT", prompts=False, fixphontrans=0, useProlog=0):
    known = {}
    unknown = {}
    xdict = {"!ENTER":"sil", "!EXIT":"sil", "sp":"sil"}
    for prompt in prompts:
        sentence0 = prompts[prompt]
        sentence1 = sentence0[:2]
        for w in sentence0[2:-1]:
            form = HTKFriendly(str(buck.uni2buck(w[FORM])))
            phones = fixPhonTrans(" ".join(str(HTKFriendly(w[DIACRITICS]))), fixphontrans)
            sentence1.append([form, phones])
        sentence1.append(sentence0[-1])
        prompts[prompt] = sentence1
    if useProlog:
        prompts = prologprep(dest, "MADA-%s"%(fixphontrans), prompts, useProlog=useProlog)
    for prompt in prompts:
        sentence0 = prompts[prompt]
        sentence1 = sentence0[:2]
        for w in sentence0[2:-1]:
            xdict[w[0]] = [w[1]]
    return xdict

def new2old(s):
    new2old = {"W":"&", "I":"<", "O":">"}
    for x in new2old:
        s = s.replace(x, new2old[x])
    return s

def old2new(s):
    new2old = {"&":"W", "<":"I", ">":"O"}
    for x in new2old:
        s = s.replace(x, new2old[x])
    return s

def fixSAMAPrompts(dest="EXPT", prompts=False, fixphontrans=0, useProlog=0):
    known = {}
    unknown = {}
    simpleprefix = re.compile("(?P<prefix>(Al)?)(I|O|W)(?P<rest>.*)")
    undiacriticise = re.compile("a|i|o|u|~|F|N|K")
    xdict = {"!ENTER":"sil", "!EXIT":"sil", "sp":"sil"}
    for prompt in prompts:
        sentence0 = prompts[prompt]
        sentence1 = sentence0[:2]
        for w in sentence0[2:-1]:
            form = w[0]
            solutions = pya.getSolutions(form)
            if solutions == []:
                incTable(form, unknown)
                solutions = [form.replace("-", "")]
            else:
                solutions = [solution.buckvoc for solution in solutions]
                incTable(form, known)
            solutions = unique([fixPhonTrans(" ".join(str(HTKFriendly(solution))), fixphontrans) for solution in solutions])
            form = HTKFriendly(form)
            sentence1.append([form, solutions[0]])
            xdict[form] = solutions
        sentence1.append(sentence0[-1])
        prompts[prompt] = sentence1
    if useProlog:
        prompts = prologprep(dest, "SAMA-%s"%(fixphontrans), prompts, useProlog=useProlog)
    print "%s distinct words were recognised by SAMA, %s were missing (%.2f); %s known tokens, %s unknown tokens (%.2f))"%(len(known), len(unknown), float(len(unknown))/float(len(known)+len(unknown)), sum(known.values()), sum(unknown.values()), float(sum(unknown.values()))/float(sum(known.values())+sum(unknown.values())))
    return xdict

def prompts2Phones(src, dest, prompts1, sep="\n"):
    """
    prompts1 is a list of lists of [FORM, PHONES] pairs like

    [[[u'*/test-SYRIANTV_NEWS25_ARB_20070319_162800-male-speaker3-native-39', 'DUMMY'], 
      ['hVA', 'h A V A'], 
      ['wyrAfq', 'w a y u r A f i q']
      ...

    (we don't care about the PHONES part of the first element, but we'll have
    it for uniformity)
    """
    phones0 = "#!MLF!#\n"
    phones1 = "#!MLF!#\n"
    allphones = set()
    for sentence1 in prompts1:
        id = sentence1[MFORM][0]
        if not id.startswith("*/"):
            id = "*/%s"%(id)
        phones0 += '"%s.lab"\nsil'%(id)
        phones1 += '"%s.lab"\nsil'%(id)
        for formphones in sentence1[2:-1]:
            form = formphones[MFORM]
            phones = space.sub(" ", formphones[MPHONES].strip())
            for p in phones.split(" "):
                allphones.add(p)
            phones0 += "%s%s"%(sep, phones.replace(" ", sep))
            phones1 += "%s%s%ssp"%(sep, phones.replace(" ", sep), sep)
        if phones1.endswith("%ssp"%(sep)):
            phones1 = phones1[:-3]
        phones0 += "%ssil%s.\n"%(sep, sep)
        phones1 += "%ssil%s.\n"%(sep, sep)
    allphones.add("sil")
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
                      
def getunrecog(ifile):
    u = set()
    for i in re.compile("\*{3}(?P<u>\S*)\*{3}").finditer(open(ifile).read()):
        u.add(i.group("u"))
    return u

def removeShortVowels(ifile):
    SVPATTERN = re.compile("\s(a|i|u)+\s")
    s = ""
    for l in open(ifile):
        if "*/" in l or "!E" in l or "sil" in l:
            s += l
        else:
            s += SVPATTERN.sub("", l)
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
    t = [l.strip() for l in open(training).read().split("\n") if not l.strip() == ""]
    if len(t) > S:
        print "Splitting training into subsections to allow us to handle large training sets"
    i = 0
    while len(t) > 0:
        i += 1
        out = "%s%s.scp"%(training.split(".")[0], i)
        lines = "\n".join(t[:S])
        if len(lines) > 0:
            print "Writing %s lines to %s"%(len(t[:S]), out)
            with safeout(out) as write:
                write("%s\n"%(lines))
        t = t[S:]

PROMPTNAME = re.compile("(.*/)?(?P<promptname>\S*)")

def countWords(prompts):
    t = 0
    for prompt in prompts:
        t += len(prompts[prompt])-3
    return t

def checkLength(prompts, destwavpath):
    t = 0.0
    for prompt in prompts:
        t += wavlength(os.path.join(destwavpath, prompt))
    return t

def savePromptsAsTXT(dest, prompts, text):
    with safeout(os.path.join(dest, text)) as write:
        for prompt in prompts:
            write(" ".join(w[FORM] for w in prompt)+"\n")

def getprompts(src, dest, madafiles, wav, testpattern, trainingpattern, minutterances, N, srcwavpath, destwavpath):
    """
    Read the test prompts, and then read the training ones making sure that you don't use a source file
    that you used for getting the test prompts
    """
    srcwavfiles = os.listdir(srcwavpath)
    makedirs(destwavpath)
    random.seed(0)
    random.shuffle(srcwavfiles)
    testprompts = {}
    i = 0
    
    """
    Get a fixed random set of test files
    """
    if testpattern:
        testpattern = re.compile(testpattern)
    print "CREATING NEW TESTSET %s"%(os.path.join(dest, "testing.txt"))
    for f in srcwavfiles:
        if f in madafiles and (not testpattern or testpattern.match(f)): 
            testprompts[f] = madafiles[f]
            i += 1
            if i == MAXTEST:
                break
    """
    Now get a consistent random set of training files of the required size
    """
    if trainingpattern:
        trainingpattern = re.compile(trainingpattern)
    trainingprompts = {}
    speakers = segments.getSpeakers(d=src, wav="wav", N=minutterances)
    print "LEN SPEAKERS %s"%(len(speakers))
    available = set()
    for f in set(madafiles.keys()).difference(set(testprompts.keys())):
        s = segments.getSpeaker(f)
        if s in speakers:
            available.add(f)
    print "len(available)=%s, len(srcwavfiles)=%s"%(len(available), len(srcwavfiles))
    i = 0
    for f in srcwavfiles:
        if f in available and (not trainingpattern or trainingpattern.match(f)):
            trainingprompts[f] = madafiles[f]
            try:
                os.link(os.path.join(srcwavpath, f), os.path.join(destwavpath, f))
            except:
                pass
            i += 1
            if i == N:
                break
    return testprompts, trainingprompts

def getWavFiles(srcwavpath, destwavpath, prompts):
    for f in prompts:
        if not os.path.exists(os.path.join(destwavpath, f)):
            try:
                os.link(os.path.join(srcwavpath, f), os.path.join(destwavpath, f))
            except:
                print "Couldn't make link from %s to %s"%(os.path.join(srcwavpath, f), os.path.join(destwavpath, f))

def toMFC(dest, wav="wav", mfcdir="mfc", training="training.scp", testing="testing.scp"):
    wavdir = os.path.join(dest, wav)
    mfcdir = os.path.join(dest, mfcdir)
    wavfiles = os.listdir(wavdir)
    makedirs(mfcdir)
    mfcfiles = os.listdir(mfcdir)
    oneNeeded = False
    print "toMFC: len(wavfiles) %s"%(len(wavfiles))
    toCopy = 0
    N = 0
    mfcPattern = re.compile("\S*/\S*/(?P<fname>\S*)\.mfc")
    with safeout(os.path.join(dest, "mfc.scp")) as write:
        for mfcfile in mfcPattern.finditer(open(os.path.join(dest, training)).read()+open(os.path.join(dest, testing)).read()):
            mfcfile = mfcfile.group("fname")
            N += 1
            if not "%s.mfc"%(mfcfile) in mfcfiles:
                write("%s/%s.wav %s/%s.mfc\n"%(wavdir, mfcfile, mfcdir, mfcfile))
                oneNeeded = True
                toCopy += 1
    print "Total required %s, need to copy %s of these"%(N, toCopy)
    if oneNeeded:
        codetrain(dest, "mfc.scp")

def prologFilterPrompts(dest, useProlog, useMada, trainingprompts, testprompts):
   print "useProlog %s"%(useProlog)
   """
   I am getting prompts (well actually lots of them) where the
   Prolog rules fail: to overcome that, I'm making prologprep be
   more careful to check that we do indeed have a text and a
   sampa form, and I'm returning the list of prompts where this
   worked as well as the dictionary
   """
   return prologprep(dest, "training", trainingprompts, useProlog=useProlog, useMada=useMada), prologprep(dest, "testing", testprompts, useProlog=useProlog, useMada=useMada)

def readImanPrompts(src="SRC", iprompts="allprompts_vowelised.txt", dest="", N=sys.maxint-50):
    SV = re.compile("a|i|o|u|~")
    prompts = []
    allwavfiles = "\n".join(os.listdir(os.path.join(src, "wav")))
    for prompt in codecs.open(os.path.join(src, iprompts), encoding="UTF-8").read().split("\n"):
        if len(prompt) > 0:
            prompt = str(space.sub(" ", buck.uni2buck(prompt.replace(u'\u060c', "")))).split()
            pattern = prompt[0][2:].split("-")
            pattern = "(?P<newname>%s.*?-%s).wav"%("-".join(pattern[:-1]), pattern[-1])
            newname = re.compile(pattern).search(allwavfiles).group("newname")
            if not " " in newname and os.path.exists(os.path.join(src, "wav", "%s.wav")%(newname)):
                newprompt = [[newname, newname], [prompt[1], prompt[1]]]
                for p in prompt[2:-1]:
                    p = HTKFriendly(p.replace("-", "").replace(".", ""))
                    if not p == "":
                        newprompt.append([SV.sub("", p), p])
                newprompt.append([prompt[-1], prompt[-1]])
                prompts.append(newprompt)
            else:
                print "No such file as %s???"%(os.path.join(src, "wav", "%s.wav")%(newname))
    print "len imanprompts %s"%(len(prompts))
    prompts = prompts[:N+50]
    testprompts = prompts[:50]
    trainingprompts = prompts[50:]
    return {"%s.wav"%(p[0][0]):p for p in testprompts}, {"%s.wav"%(p[0][0]):p for p in trainingprompts}

"""
Make the basic dictionaries and the initial transcriptions. This may be overwritten later if we decide to apply fixphontrans
"""

def saveMonophones(prompts):
    phones = set(["sil", "ENTER!", "EXIT!"])
    for prompt in prompt:
        for word in prompt:
            for phone in word[1]:
                phones.add(phone)

"""
It can easily happen with multi-dictionaries that later
versions of a word contain a phoneme that does not
appear in the first version of any word; we have to delete
these because they cause problems later on
"""
def checkDictAgainstPhones(xdict, phones):
    for word in xdict:
        transcriptions1 = []
        for t in xdict[word]:
            if set(t).intersection(phones) == set(t):
                transcriptions1.append(t)
            else:
                "%s has a phone that is not in phones"%(t)
        xdict[word] = transcriptions1
    
def initDictsAndPrompts(src, dest, version, fixphontrans, useProlog, testprompts, trainingprompts, N):
    print "VERSION %s, FIXPHONTRANS %s"%(version, fixphontrans)
    if version == "LDC":
        tdict0 = fixLDCPrompts(dest=dest, prompts=testprompts, fixphontrans=fixphontrans, useProlog=useProlog)
        phondict1 = fixLDCPrompts(dest=dest, prompts=trainingprompts, fixphontrans=fixphontrans, useProlog=useProlog)
    elif version == "SAMA":
        phondict1 = fixSAMAPrompts(dest=dest, prompts=trainingprompts, fixphontrans=fixphontrans, useProlog=useProlog)
        tdict0 = fixSAMAPrompts(dest=dest, prompts=testprompts, fixphontrans=fixphontrans, useProlog=useProlog)
    elif version == "RAW":
        phondict1 = fixRAWPrompts(dest=dest, prompts=trainingprompts, fixphontrans=fixphontrans, useProlog=useProlog)
        tdict0 = fixRAWPrompts(dest=dest, prompts=testprompts, fixphontrans=fixphontrans, useProlog=useProlog)
    elif version == "MADA":
        phondict1 = fixMadaPrompts(dest=dest, prompts=trainingprompts, fixphontrans=fixphontrans, useProlog=useProlog)
        tdict0 = fixMadaPrompts(dest=dest, prompts=testprompts, fixphontrans=fixphontrans, useProlog=useProlog)
    else:
        raise Exception("Unknown transcription method: %s"%(version))
    for k in tdict0:
        phondict1[k] = tdict0[k]
    phondict0 = {}
    for k in phondict1:
        phondict0[k] = phondict1[k]
    phones = set()
    for transcriptions in phondict0.values():
        for phone in transcriptions[0]:
            phones.add(phone)
    prompts2Phones(src, dest, trainingprompts.values()+testprompts.values())
    return phondict0, phondict1, tdict0
              
def dataprep(src, dest, expt=False, prompts="originalprompts.txt", makeGrammar=makeBigramGrammar, version="MADA:%s"%(BASE), useProlog=0, useFixedDict='multi', wav="wav", N=sys.maxsize, useconfig=useconfig25, multiStage="yes", testpattern=False, trainingpattern=False, savetestset=True, minutterances=20):
    useconfig()
    m = VERSION.match(version)
    version, fixphontrans = m.group("version"), m.group("fixphontrans")
    try: 
        fixphontrans = int(fixphontrans)
    except:
        fixphontrans = 0
    if not dest:
        dest = os.path.join(src, "EXPT-%s-%s"%(prompts, datetime.now().strftime("%b-%d@%H:%M")))
    makedirs(dest)
    with safeout(os.path.join(dest, "mfcconfig.txt")) as write:
         write(MFCCONFIG)
    with safeout(os.path.join(dest, "config.txt")) as write:
        write(CONFIG)
    destwavpath = os.path.join(dest, wav)
    """
    Check that every prompt is indeed a prompt and that there is a wav file for it
    """
    srcwavpath = os.path.join(src, wav)
    if version == "IMAN":
        testprompts, trainingprompts = readImanPrompts(N=N)
    else:
        madafiles = segments.readMadaFiles(os.path.join(src, "madafiles.json"))
        testprompts, trainingprompts = getprompts(src, dest, madafiles, wav, testpattern, trainingpattern, minutterances, N, srcwavpath, destwavpath)
    getWavFiles(srcwavpath, destwavpath, testprompts)
    getWavFiles(srcwavpath, destwavpath, trainingprompts)
    print "length of recordings for testing %.2f (%s words)"%(checkLength(testprompts, destwavpath)/60.0, countWords(testprompts))
    trainingtime = checkLength(trainingprompts, destwavpath)/60.0
    print "length of recordings for training %.2f (%s words)"%(trainingtime, countWords(trainingprompts))
    mfcdir="mfc-%s"%(useconfig.__name__)
    allprompts = "allprompts"
    training = "training.txt"
    testing = "testing.txt"
    wordsmlf(dest, trainingprompts.values()+testprompts.values(), words="words.mlf")
    wordsmlf(dest, testprompts.values(), words="testwords.mlf")
    phondict0, phondict1, tdict0 = initDictsAndPrompts(src, dest, version, fixphontrans, useProlog, testprompts, trainingprompts, N)
    if expt:
        expt.testing = testprompts
        expt.training = trainingprompts
        expt.phondict = phondict0
        expt.trainingtime = trainingtime
        expt.version = version
        expt.fixphontrans = fixphontrans
    return writeDictionariesAndPrompts(dest, wav, mfcdir, expt, "%s:%s"%(version, fixphontrans), useFixedDict, multiStage, N, phondict0, phondict1, tdict0, makeGrammar, trainingprompts, training, testprompts, testing)
    
def writeDictionariesAndPrompts(dest, wav, mfcdir, expt, useMada, useFixedDict, multiStage, N, phondict0, phondict1, tdict0, makeGrammar, trainingprompts, training, testprompts, testing):
    htkformat_dictionary(phondict0, dest, "dict-%s-%s-%s.txt"%(useMada, N, 0), useFixedDict=useFixedDict)
    multipleEntries = htkformat_dictionary(phondict1, dest, "dict-%s-%s-%s.txt"%(useMada, N, 1), D=1, useFixedDict=useFixedDict)
    if multiStage == "yes":
        shutil.copy(os.path.join(dest, "phones1.mlf"), os.path.join(dest, "phones2.mlf"))
        shutil.copy(os.path.join(dest, "dict-%s-%s-1.txt"%(useMada, N)), os.path.join(dest, "dict-%s-%s-2.txt"%(useMada, N)))
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
    tdict1 = tdict0
    htkformat_dictionary(tdict0, dest, "testdict-0.txt", useFixedDict=useFixedDict)
    htkformat_dictionary(tdict1, dest, "testdict-1.txt", D=1, useFixedDict=useFixedDict)
    makeGrammar(dest, testprompts.values(), dfile="testdict-0.txt")
    makeGrammar(dest, testprompts.values(), dfile="testdict-0.txt", wmlf="testwords")
    print "toMFC(%s, wav=%s, mfcdir=%s)"%(dest, wav, mfcdir)
    toMFC(dest, wav=wav, mfcdir=mfcdir)
    return multipleEntries

####################################################################################
# General initialisation stuff: copy wav files & prompts, make .scp and .mlf files #
####################################################################################

def train(d='EXPT', expt=False, training='training.scp', testing='testing.scp', useProlog=False, version=False, useForcedAlignment='Y', localtests=50, useTiedList=True, rounds=3, findmismatches=False, phondict=False, quit=100, multiStage="no", useFixedDict="multi", mfcdir="mfc", N=sys.maxint, useconfig=useconfig25):
    T = datetime.now()
    print "TRAINING STARTED %s: useForcedAlignment=%s, useProlog=%s, version=%s, multiStage=%s"%(T, useForcedAlignment, useProlog, version, multiStage)
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
        lastdict = "dict-%s-%s-0.txt"%(version, N)
        if HERest(d, i, 0):
            test(d, hmm="hmm%s"%(i), hmmlist="monophones0", lexicon=testdict, N=localtests)
            if i > quit:
                return {}
    i += 1
    addShortPauses(d, i)
    i += 2
    testdict = "testdict-1.txt"
    print "TRAINING WITH MONOPHONES1 (i.e. with short pauses this time): %s rounds"%(rounds)
    inc = 0 if multiStage=="yes" else 2
    inc = 0
    for i in range(i, i+rounds+inc): # range(6, 8)
        lastdict = "dict-%s-%s-1.txt"%(version, N)
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
        lastdict = "dict-%s-%s-2.txt"%(version, N)
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
        aligned(d, "trainingFA.scp", i, lexicon=lastdict, mfcdir=mfcdir)
        checkDictOK(d, dict=testdict)
        print "TRAINING AFTER FORCED ALIGNMENT"
    else:
        print "Skipping realignment -- phones1 was derived by using context sensitive rules, so is supposed to have the right version at each point already"
        shutil.copyfile("%s/phones1.mlf"%(d), "%s/aligned.mlf"%(d))
        shutil.copyfile("%s/training.scp"%(d), "%s/trainingFA.scp"%(d))
    splitTraining(os.path.join(d, "trainingFA.scp"), S=10000)
    i += 1
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
    if useTiedList and not multiStage.startswith("y"):
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
    flags = {"useProlog":useProlog, "version":version, "useFixedDict":useFixedDict, "multiStage":multiStage, "useForcedAlignment":useForcedAlignment, "N":N, "useconfig":useconfig.__name__}
    print "TRAINING WITH %s TOOK %s SECONDS\n"%(flags, int((T1-T).total_seconds()))
    if quit == 99:
        return {}
    alignments = test(d, hmm=hmm, hmmlist=hmmlist, lexicon=testdict, name="-%s-%s"%(version, useProlog))
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
                    print "Eliminating %s from %s because %s is not in phones"%(line, dict, x)
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
    
def test(d, hmm='hmm15', hmmlist='tiedlist', lexicon="dict-tri", testing="testing.txt", o="", N=sys.maxsize, name="", wdnet="wdnet-testwords"):
    if N == 0:
        return
    hmmrecout = "recout%s.mlf"%(hmm)
    print recout(d, hmm=hmm, hmmlist=hmmlist, lexicon=lexicon, N=N, recout=hmmrecout, o=o, wdnet=wdnet)
    recoutlines = [(i.group("TEST"), cleanWords(splitWords(i.group("WORDS").strip()))) for i in re.compile('"(?P<TEST>\S*)"(?P<WORDS>.*?)\n\.', re.DOTALL).finditer(open(os.path.join(d, hmmrecout)).read())]
    return showAlignment(d, recoutlines, testing=testing, recout=hmmrecout, name=name)
    
def readAlignments(d, alignment):
    return [i.group("word") for i in re.compile("#### \('(?P<word>\S*)', '\S*'\) ####").finditer(open(os.path.join(d, alignment)).read())]

def wordcount(f):
    return execute("wc %s"%(f))[0].split()[1]
    
def recout(d, hmm, hmmlist, lexicon, config="config.txt", N=10000, recout="recout.mlf", wdnet="wdnet-testwords", o=" -o SWT", scp="temp.scp"):
    with safeout(os.path.join(d, "temp.scp")) as write:
        write("\n".join(readPrompts(os.path.join(d, "testing.scp"))[:N]))
    wdnet = os.path.join(d, wdnet)
    lexicon = os.path.join(d, lexicon)
    scp = os.path.join(d, scp)
    print "IN RECOUT USING %s (%s), %s, (%s), %s (%s)"%(wdnet, wordcount(wdnet), lexicon, wordcount(lexicon), scp, wordcount(scp))
    execute('HVite -A -D -T %s -p 2.0 -s 10.0 -H %s -H %s -C %s -S %s -i %s -w %s %s %s %s'%(DEBUG, os.path.join(d, hmm, "macros"), os.path.join(d, hmm, "hmmdefs"), os.path.join(d, config), scp, os.path.join(d, recout), wdnet, o, lexicon, os.path.join(d, hmmlist)))
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

def aligned(dest, newtraining, i, lexicon="dict1.txt", mfcdir="mfc"):
    hmm = os.path.join(dest, "hmm%s"%(i))
    print "aligned(%s, %s)"%(hmm, lexicon)
    testpattern = re.compile('"*/(?P<test>.*).lab"')
    standard = "HVite -A -D -T %s -p 2.0 -s 10.0 -l * -o SWT -a -b !EXIT -m -C %s/config.txt -H %s/macros -H %s/hmmdefs -i %s/aligned.mlf -t 250.0 150.0 1000.0 -y lab -I %s/words.mlf -S %s/training.scp %s/%s %s/monophones1"%(DEBUG, dest, hmm, hmm, dest, dest, dest, dest, lexicon, dest)
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
    print "About to write trainingFA.scp: %s"%(os.path.join(dest, newtraining))
    with safeout(os.path.join(dest, newtraining)) as write:
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
    allphones = set()
    """
    for phone in open('%s/fulllist'%(d)).readlines()+open('%s/triphones1'%(d)).readlines():
        allphones[phone] = True
    """
    for phone in open('%s/fulllist'%(d)).readlines()+open('%s/triphones1'%(d)).readlines():
        allphones.add(phone)
    with safeout('%s/fulllist'%(d)) as write:
        for phone in sorted(allphones):
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
    
def experiment(src="SRC", dest="TEST", prompts="", dicts=["multi", "fixed"], prolog=0, stages="no", versions="MADA%s"%(0), forcedAlignment=["Y", "N"], configs=useconfig39, N=sys.maxsize, localtests=10, maintests=100, quit=100, testpattern=False, trainingpattern=False, savetestset=True):
    makedirs(dest)
    makeGrammar = makeGrammar1
    makeGrammar = makeBigramGrammar
    prolog = enlist(prolog)
    dicts = enlist(dicts)
    versions = enlist(versions)
    stages = enlist(stages)
    forcedAlignment = enlist(forcedAlignment)
    configs = enlist(configs)
    maintests = enlist(maintests)
    N = enlist(N)
    experiments = []
    clean(dest)
    global MAXTEST
    for useconfig in configs:
        for useFixedDict in dicts: 
            for useForcedAlignment in forcedAlignment:
                if useFixedDict == "fixed" and useForcedAlignment == "Y":
                    print "Skipping useForcedAlignment=%s, useFixedDict=%s"%(useForcedAlignment, useFixedDict)
                    continue
                for version in versions: 
                    if version.startswith("base") and useFixedDict=="multi":
                        print "Skipping version=%s, useFixedDict=%s"%(version, useFixedDict)
                        continue
                    for useProlog in prolog:
                        for multiStage in stages:
                            if multiStage == "yes" and not version.startswith("madamira"):
                                print "Skipping version=%s, multiStage=%s"%(version, multiStage)
                                continue
                            try:
                                os.remove(os.path.join(dest, "wdnet"))
                                print "REMOVED %s AT START OF CYCLE"%(os.path.join(dest, "wdnet"))
                            except:
                                pass
                            for n in N:
                                for MAXTEST in maintests:
                                    print "#### EXPERIMENT %s ########"%(fixflags({"useProlog":useProlog, "version":version, "useFixedDict":useFixedDict, "multiStage":multiStage, "useForcedAlignment":useForcedAlignment, "N":n, "config":useconfig.__name__}))
                                    clean(dest)
                                    experiments.append(EXPERIMENT(src, dest, prompts, version, useProlog, useFixedDict, useconfig, makeGrammar, useForcedAlignment, localtests, multiStage=multiStage, N=n, quit=quit, testpattern=testpattern, trainingpattern=trainingpattern, savetestset=savetestset))
                                    if len(experiments[-1].training) < n:
                                        break
    return experiments

class EXPERIMENT:

    def __init__(self, src, dest, prompts, version, useProlog, useFixedDict, useconfig, makeGrammar, useForcedAlignment, localtests, N=sys.maxsize, quit=100, multiStage="no", testpattern=False, trainingpattern=False, savetestset=True):
        self.src = src
        self.dest = dest
        self.prompts = prompts
        self.useProlog = useProlog
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
        m = dataprep(src, dest, expt=self, prompts=prompts, useProlog=useProlog, version=version, useFixedDict=useFixedDict, useconfig=useconfig, makeGrammar=makeGrammar, N=N, multiStage=self.multiStage, testpattern=testpattern, trainingpattern=trainingpattern, savetestset=savetestset)
        if m == 0 and useForcedAlignment == "Y":
            print "No point in doing forced alignment if there aren't multiple entries in the dictionary"
            return
        """
        Then do training
        """
        train(dest, expt=self, useProlog=useProlog, version=version, useForcedAlignment=useForcedAlignment, localtests=localtests, phondict=self.phondict, quit=quit, multiStage=self.multiStage, mfcdir="mfc-%s"%(useconfig.__name__), N=N, useconfig=useconfig, useFixedDict=useFixedDict) 
        print "SCORE AS CSV %s\t%.2f\t%.3f\n"%(len(self.training), self.trainingtime, self.score()[0])

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
    results = {}
    minL = None
    labels = set()
    for e in expts:
        try:
            label = "%s:%s:%s:%s:F%s:P%s"%(e.version[0], e.fixphontrans, e.useFixedDict[0], e.useconfig.__name__.replace("useconfig", "c"), e.useForcedAlignment, e.useProlog) 
            l = len(e.training)
            t = float("%.2f"%(e.trainingtime))
            s = "%.3f"%(e.score()[0])
            result = [l, s]
            labels.add(label)
            if not t in results:
                results[t] = {}
            results[t][label] = result
        except:
            pass
    for r in results.values():
        for l in labels:
            if not l in r:
                r[l] = [0, '']
    with safeout(out, mode="a") as write:
        write("training time\t%s\n"%("\t".join(sorted(labels))))
        for t in sorted(results.keys()):
            row = results[t]
            write("%s\t%s\n"%(t, "\t".join(row[x][1] for x in sorted(row.keys()))))
    return results

def textresults2csv(s, out=sys.stdout):
    if not "\n" in s:
        s = open(s).read()
    with safeout(out) as write:
        for r in re.compile("SCORE AS CSV (?P<L>\S+)	(?P<T>\S+)	(?P<A>\S+)").finditer(s):
            write("%s\t%.2f\t%.3f\n"%(r.group("L"), float(r.group("T")), float(r.group("A"))))

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

