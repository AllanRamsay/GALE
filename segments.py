import re, sys, os
import buck
from useful import *
from shared import *
import os, sys, shutil
import sounds
reload(sounds)
import json

"""
We want segments transliterated using old-style Buckwalter, because
we're going to feed them to PYA, which doesn't want new-style
"""

HERE = os.getcwd()
MADAMIRAHOME = HERE
TDF = os.path.join(HERE, "gale_p3_ara_bn_transcripts_p1/data/tdf")
WAV = os.path.join(HERE, "gale_p3_bn_speech_p1/data")
TESTPROMPT = "SYRIANTV_NEWS25_ARB_20070403_162800.qrtr.tdf"

P = re.compile("^(?P<prompt>\S*)	(?P<channel>\d+)	(?P<start>[\d.]+)	(?P<end>[\d.]+)	(?P<speaker>.*)	(?P<gender>\S*)	(?P<dialect>\S*)	(?P<transcript>.*)	(?P<section>\d+)	(?P<turn>\d+)	(?P<segment>-?\d+)	(?P<secType>\S*)(\s*(?P<uType>\S+))?$")

rubbish = re.compile("Dialecte non trans")

tag = re.compile("<.*?>")
respace = re.compile("\s+")
brackets = re.compile("\(|\)|=|\+")

def segment(src=os.path.join(TDF, TESTPROMPT), dest="TEMP", wav=WAV, N=sys.maxint, segments=False, prompts="", promptsfile="originalprompts.segments", useBW=False, rawPrompts=True, copywavfiles=False, separatedashes=True):
    print "SRC %s, N %s"%(src, N)
    if segments == False:
        segments = []
        if copywavfiles:
            try:
                os.makedirs(os.path.join(copywavfiles,"wav"))
            except:
                print "%s already exists"%(os.path.join(copywavfiles,"wav"))
    segments = []
    if os.path.isdir(src):
        for f in os.listdir(src):
            if N > 0:
                N, prompts = segment(src=os.path.join(src, f), dest=dest, wav=wav, N=N, segments=segments, prompts=prompts)
    else:
        prompt = src.split("/")[-1]
        if not prompt.endswith(".qrtr.tdf"):
            return N, prompts
        print prompt
        prompt = prompt[:-len(".qrtr.tdf")]
        if copywavfiles:
            sound = sounds.readsound(os.path.join(copywavfiles, wav, "%s.wav"%(prompt)))
        for i, line in enumerate(open(src)):
            line = line.strip()
            m = P.match(line.strip())
            if m and not rubbish.search(line):
                if N <= 0:
                    break
                N -= 1
                transcript = m.group("transcript").decode("UTF-8")
                if useBW:
                    transcript = buck.uni2buck(transcript, buck._uni2buck)
                if separatedashes:
                    transcript = transcript.replace("-", " - ")
                transcript = respace.sub(" ", brackets.sub("", tag.sub("", transcript)))
                s = [m.group("prompt"), float(m.group("start")), float(m.group("end")), transcript]
                test ="test-%s-%s-%s-%s-%s"%(prompt, m.group("gender"), m.group("speaker"), m.group("dialect"), i)
                if transcript.strip() == "":
                    continue
                if rawPrompts:
                    prompts += "%s\n"%(transcript)
                else:
                    try:
                        prompts += "*/%s !ENTER %s !EXIT\n"%(str(test), transcript)
                        if copywavfiles:
                            segments.append(s)
                            start = s[1]
                            end = s[2]
                            w = os.path.join(copywavfiles, "wav", "%s.wav"%(test))
                            if not os.path.isfile(w):
                                sound.frames = False
                                sound.save(w, start=int(start*sound.params[2]), end=int(end*sound.params[2]))
                    except:
                        pass
    return N, prompts

def saveprompts(prompts, out):
    print "Saving into %s"%(out)
    with safeout(out, encoding="UTF-8") as write:
        write(prompts)

def getPrompts(src=os.path.join(TDF, TESTPROMPT), dest="SRC", promptsfile="originalprompts.segments", rawPrompts=False, useBW=False, out=False, runMadamira=True, N=sys.maxint, copywavfiles=False):
    N, prompts = segment(src, dest=dest, rawPrompts=rawPrompts, useBW=useBW, N=N, copywavfiles=copywavfiles)
    if not out:
        out = promptsfile
    out = os.path.join(dest, out)
    print "len(prompts)=%s"%(len(prompts))
    saveprompts(prompts, out)
    if runMadamira:
        runmadamira(src=dest, dest=dest)
    return N

def getPromptsLocally(src=TDF, dest="SRC", rawPrompts=False, useBW=False, runMadamira=False, N=sys.maxint, copywavfiles=False):
    for path, dirs, files in os.walk(src):
        for f0 in files:
            f1 = os.path.join(dest, f0.split(".")[0])
            try:
                os.makedirs(f1)
            except:
                pass
            N = getPrompts(src=os.path.join(src, f0), dest=f1, rawPrompts=rawPrompts, useBW=useBW, runMadamira=runMadamira, N=N, copywavfiles=copywavfiles)
            if N <= 0:
                return

def tdf2bw(ifile=os.path.join(TDF, TESTPROMPT)):
    with safeout(ifile+".bw") as write:
        s = open(ifile).read()
        s = a2bw.convert(s.decode("UTF-8"))
        write(s)

def runmadamira(madamiradir=MADAMIRAHOME, src="TEMP", dest="TEMP", prompts="originalprompts.segments", convertBW=False):
    print 'runmadamira(madamiradir=%s, src="%s", dest="%s")'%(madamiradir, src, dest)
    where = "%s/%s"%(src, prompts)
    if convertBW:
        print "converting %s to Arabic"%(where)
        s = buck.buck2uni(open(where).read())
        where = "%s.uni"%(where)
        with safeout(where, encoding="UTF-8") as write:
            write(s)
    execute("java -Xmx2500m -Xms2500m -XX:NewRatio=3 -jar MADAMIRA-release-20170403-2.1.jar -rawinput %s -rawoutdir %s"%(where, dest), d=madamiradir)

"""
Copy the diacritics from the following word. If you hit the next
element of the form of the preceding one, move on. If this was the end
of the preceding one then quit.
"""

def borrowDiacritics(w0, w1):
    d = u""
    b = buck.uni2buck(w0.form)
    for i, c in enumerate(w1[DIACRITICS]):
        d += c
        if c == b[0]:
            b = b[1:]
            if b == "":
                if len(d) == 1:
                    try:
                        d += w1[DIACRITICS][i+1]
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
        if w[FORM] == "-":
            try:
                w0, w1 = sentence0[i-1], sentence0[i+1]
                n += 1
                """
                We're interested in the word before the dash. If that was "Al" 
                we will just fix the diacritics to be "Qal", otherwise we will
                try to borrow them from the next word
                """
                if w0[DIACRITICS] == "Al":
                    w0[DIACRITICS] = "Qal"
                elif w1[FORM].startswith(w0[FORM]):
                    """ 
                    bgt- bgtdq
                    Mada said that bgt was bagat but that bgtdq was bugutidaq
                    Because the false start looks like the next word, we use the next word's
                    diacritics, so for bgt we do bugut
                    """
                    w0[DIACRITICS] = borrowDiacritics(w0, w1)
                elif len(sentence[i-1][FORM]) == 1:
                    w0[DIACRITICS] = w0[FORM]+"a"
                w0[FORM] = w0[FORM]+"-"
            except Exception as e:
                pass
            """
            Whatever happens, we will omit the dash from the transcription
            """
            continue
        else:
            sentence1.append(w)
    """
    # Don't do HTKFriendly yet, because SAMA doesn't like it
    for w in sentence1[2:-1]:
        w[FORM] = HTKFriendly(w[FORM])
    """
    return sentence1

sPattern = re.compile(""";;; SENTENCE \S*(?P<test>test\S*)(?P<words>.*?)
--------------
SENTENCE BREAK
--------------
""", re.DOTALL)
WPATTERN = re.compile("WORD (@@LAT@@)?(?P<word>\S*)\s.*?(--------------|$)", re.DOTALL)
DPATTERN = re.compile(".*SVM_PREDICTIONS:\s*(@@LAT@@)?\S*\s*diac:(?P<diac>\S*)\s.*?((?P<gloss>gloss:\s*\S*)|(?P<none>NO-ANALYSIS)).*", re.DOTALL)

class foreign(Exception):

    def __init__(self, msg):
        self.msg = msg

    def __repr__(self):
        return "foreign('%s')"%(self.msg)

import codecs
def mergeMadaFiles(top=".", out=False):
    madafiles = {}
    for d, p, files in os.walk(top):
        for f in files:
            if f == "originalprompts.segments.mada":
                for s in sPattern.finditer(codecs.open(os.path.join(d, f), encoding="UTF-8").read()):
                    try:
                        words = []
                        for w in WPATTERN.finditer(s.group("words")):
                            try:
                                form = str(buck.uni2buck(w.group("word")))
                                d = DPATTERN.match(w.group(0))
                                diac = str(buck.uni2buck(d.group("diac")))
                            except Exception as e:
                                print "Couldn't do conversion"
                                print w.group(0)
                                print buck.uni2buck(w.group("word"))
                                raise e
                            try:
                                gloss = str(d.group("gloss"))
                            except:
                                gloss = d.group("none")
                            if "foreign" in form:
                                raise foreign(form)
                            if re.compile(".*\d.*").match(form) and not form.startswith("*/test"):
                                raise foreign(form)
                            words.append([form, diac, gloss])
                    except foreign:
                        continue
                    if len(words) > 3:
                        madafiles[str("%s.wav"%(s.group("test")))] = fixDashes(words)
    if out:
        out = open(os.path.join(top, out), "w")
        json.dump(madafiles, out)
        out.close()
    return madafiles

def readMadaFiles(madafiles):
    return json.load(open(madafiles))

"""
test-ABUDHABI_ABUDHNEWS_ARB_20070324_115800-male-speaker9-native-369.wav
"""

SPEAKERPATTERN = re.compile("test-(?P<file>.*)-(?P<gender>.*)-(?P<speaker>.*)-(?P<dialect>.*)-(?P<id>.*).wav")

def getSpeaker(f):
    try:
        m = SPEAKERPATTERN.match(f)
        return "%s-%s"%(m.group("file"), m.group("speaker"))
    except Exception as e:
        return False

def getSpeakers(d="SRC", wav="wav", N=0):
    speakers = {}
    for f in os.listdir(os.path.join(d, wav)):
        s = getSpeaker(f)
        if s:
            try:
                speakers[s] += 1
            except:
                speakers[s] = 1
    for s in speakers.keys():
        if speakers[s] < N:
            del speakers[s]
    return speakers
