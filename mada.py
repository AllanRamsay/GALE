#-*- coding: utf-8 -*-

import re, sys, os, shutil, codecs
import a2bw
reload(a2bw)
import buck
from useful import *
import pyaramorph as pya
reload(pya)
from HTK import readRawMada

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

# Here's the pattern

SPATTERN = re.compile(";;; SENTENCE(?P<sentence>.*?)SENTENCE BREAK", re.DOTALL)
WPATTERN = re.compile("WORD.*?--------------", re.DOTALL)
DPATTERN = re.compile(".*SVM_PREDICTIONS:\s*(@@LAT@@)?(?P<word>\S*)\s*diac:(?P<diac>\S*)\s.*?((?P<gloss>gloss:\s*\S*)|(?P<none>NO-ANALYSIS)).*", re.DOTALL)

class MADASOLUTION:

    def __init__(self, form, diacritics, gloss):
        self.form = form
        self.diacritics = diacritics
        self.gloss = gloss

    def __repr__(self):
        return "%s:%s:%s"%(str(self.form.encode("UTF-8")), str(self.diacritics), str(self.gloss))

def getUnknowns(ifile="XXX/ABUDHABI_ABUDHNEWS2_ARB_20070228_000000/originalprompts.segments.mada", useBW=False, samaknowns=False, madaknowns=False, samaunknowns=False, madaunknowns=False, sentences=False):
    """ 
    Read it as UTF-8, split it into SENTENCEs
    """
    if samaunknowns == False:
        samaunknowns = {}
    if madaunknowns == False:
        madaunknowns = {}
    if samaknowns == False:
        samaknowns = {}
    if madaknowns == False:
        madaknowns = {}
    if sentences == False:
        sentences = []
    sentences = []
    for sentence in readRawMada(ifile):
        """
        Get all examples of the pattern, dig out the bit we want
        """
        dforms = []
        for w in sentence:
            form = w.form
            if "!E" in form or "*/test" in form:
                continue
            diacritics = w.diacritics
            mgloss = w.gloss
            if mgloss == None:
                mgloss = "***"
                try:
                    madaunknowns[form] += 1
                except:
                    madaunknowns[form] = 1
            else:
                try:
                    madaknowns[form] += 1
                except:
                    madaknowns[form] = 1
            pyasolutions = pya.getSolutions(form)
            try:
                pyasolution, pyagloss = pyasolutions[0].buckvoc, pyasolutions[0].gloss_b
            except:
                pyasolution, pyagloss = "***", ""
                try:
                    samaunknowns[form] += 1
                except:
                    samaunknowns[form] = 1
            else:
                try:
                    samaknowns[form] += 1
                except:
                    samaknowns[form] = 1
            if useBW:
                x = "%s\t%s\t%s\t%s\t%s\t%s"%(form, buck.uni2buck(form), buck.uni2buck(diacritics), mgloss, pyasolution, pyagloss)
            else:
                x = "%s\t%s\t%s\t%s\t%s\t%s"%(form, form, diacritics, mgloss, pyasolution, pyagloss)
            dforms.append(x)
        sentences.append(dforms)

def getAllUnknowns(d):
    samaknowns = {}
    madaknowns = {}
    samaunknowns = {}
    madaunknowns = {}
    sentences = []
    for f in os.listdir(d):
        try:
            getUnknowns(os.path.join(d, f, "originalprompts.segments.mada"), samaunknowns=samaunknowns, madaunknowns=madaunknowns, samaknowns=samaknowns, madaknowns=madaknowns, sentences=sentences)
        except:
            pass
    return madaunknowns, samaunknowns, madaknowns, samaknowns, sentences

def saveUnknowns(madaunknowns, samaunknowns, out=sys.stdout):
    with safeout(out, encoding="UTF-8") as write:
        write("form\tmada\tsama\n")
        for k in sorted(set(madaunknowns.keys()+samaunknowns.keys())):
            try:
                m = madaunknowns[k]
            except:
                m = ""
            try:
                s = samaunknowns[k]
            except:
                s = ""
            write("%s\t%s\t%s\n"%(k, m, s))

def getunrecog(ifile):
    unrec0 = {}
    rec0 = {}
    unrec1 = {}
    rec1 = {}
    for i in open(ifile):
        if "!E" in i or "test" in i:
            continue
        i = i.strip().split()
        if i[-1] == "***":
            try:
                unrec0[i[0]] += 1
            except:
                unrec0[i[0]] = 1
        else:
            try:
                rec0[i[0]] += 1
            except:
                rec0[i[0]] = 1
        if i[-2] == "***":
            try:
                unrec1[i[0]] += 1
            except:
                unrec1[i[0]] = 1
        else:
            try:
                rec1[i[0]] += 1
            except:
                rec1[i[0]] = 1
    print "%s tokens out of %s are unrecognised by SAMA"%(sum(unrec0.values()), sum(unrec0.values())+sum(rec0.values()))
    print "%s distinct words out of %s are unrecognised by SAMA"%(len(unrec0.values()), len(unrec0.values())+len(rec0.values()))
    print "%s tokens out of %s are unrecognised by MADAMIRA"%(sum(unrec1.values()), sum(unrec1.values())+sum(rec1.values()))
    print "%s distinct words out of %s are unrecognised by MADAMIRA"%(len(unrec1.values()), len(unrec1.values())+len(rec1.values()))
    return unrec0, rec0, unrec1, rec1
