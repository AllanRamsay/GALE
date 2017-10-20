import sys, re, os
from useful import *

def countbigrams(ifile="EXPTX/allprompts.txt"):
    bg = {}
    for l in open(ifile):
        l = l.split()[2:-1]
        for i in range(len(l)-1):
            incTable("%s-%s"%(l[i], l[i+1]), bg)
    return bg
            
