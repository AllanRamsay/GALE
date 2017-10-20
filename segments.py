import re, sys, os
import a2bw
reload(a2bw)
import buck
from useful import *
import os, sys, shutil
import sounds
reload(sounds)

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

tag = re.compile("<\S*>")
respace = re.compile("\s+")
brackets = re.compile("\(|\)|=|\+")

def segment(src=os.path.join(TDF, TESTPROMPT), dest="TEMP", wav=WAV, N=sys.maxint, segments=False, prompts="", promptsfile="originalprompts.segments", useBW=False, rawPrompts=True, copywavfiles=False):
    print "SRC %s, N %s"%(src, N)
    if segments == False:
        segments = []
        try:
            os.makedirs(os.path.join(dest,"wav"))
        except:
            print "%s already exists"%(os.path.join(dest,"wav"))
    segments = []
    if os.path.isdir(src):
        for f in os.listdir(src):
            if N > 0:
                N, prompts = segment(src=os.path.join(src, f), dest=dest, wav=WAV, N=N, segments=segments, prompts=prompts)
    else:
        prompt = src.split("/")[-1]
        if not prompt.endswith(".qrtr.tdf"):
            return N, prompts
        prompt = prompt[:-len(".qrtr.tdf")]
        if copywavfiles:
            sound = sounds.readsound(os.path.join(wav, "%s.wav"%(prompt)))
        for i, line in enumerate(open(src)):
            m = P.match(line.strip())
            if m:
                N -= 1
                if N == 0:
                    break
                print i, line
                transcript = m.group("transcript").decode("UTF-8")
                if useBW:
                    transcript = a2bw.convert(transcript, buck._uni2buck)
                transcript = respace.sub(" ", brackets.sub("", tag.sub("", transcript)))
                s = [m.group("prompt"), float(m.group("start")), float(m.group("end")), transcript]
                test ="test-%s-%s"%(prompt, i)
                if transcript.strip() == "":
                    continue
                if rawPrompts:
                    prompts += "%s\n"%(transcript)
                else:
                    prompts += "*/%s !ENTER %s !EXIT\n"%(test, transcript)
                segments.append(s)
                start = s[1]
                end = s[2]
                if copywavfiles:
                    sound.frames = False
                    w = os.path.join(dest, "wav", "%s.wav"%(test))
                    sound.save(w, start=int(start*sound.params[2]), end=int(end*sound.params[2]))
    return N, prompts

def saveprompts(prompts, out):
    print "Saving into %s"%(out)
    with safeout(out, encoding="UTF-8") as write:
        write(prompts)

def getPrompts(src=os.path.join(TDF, TESTPROMPT), dest="TEMP", promptsfile="originalprompts.segments", rawPrompts=False, useBW=False, out=False, runMadamira=True, N=sys.maxint):
    N, prompts = segment(src, rawPrompts=rawPrompts, useBW=useBW, N=N)
    if out:
        out = os.path.join(dest, out)
    else:
        out = os.path.join(dest, promptsfile)
    saveprompts(prompts, out)
    if runMadamira:
        runmadamira(src=dest, dest=dest)
    return N

def getPromptsLocally(src=TDF, dest="TEMP", rawPrompts=True, useBW=False, out=False, runMadamira=False, N=sys.maxint):
    for path, dirs, files in os.walk(src):
        for f0 in files:
            f1 = os.path.join(dest, f0.split(".")[0])
            try:
                os.makedirs(f1)
            except:
                pass
            N = getPrompts(src=os.path.join(src, f0), dest=f1, rawPrompts=rawPrompts, useBW=useBW, runMadamira=runMadamira, N=N)
            if N <= 0:
                return

def tdf2bw(ifile=os.path.join(TDF, TESTPROMPT)):
    with safeout(ifile+".bw") as write:
        s = open(ifile).read()
        s = a2bw.convert(s.decode("UTF-8"))
        write(s)

def runmadamira(madamiradir=MADAMIRAHOME, src="TEMP", dest="TEMP"):
    print "HERE %s"%(HERE)
    execute("java -Xmx2500m -Xms2500m -XX:NewRatio=3 -jar MADAMIRA-release-20170403-2.1.jar -rawinput %s/originalprompts.segments -rawoutdir %s"%(src, dest), d=madamiradir)
                    
