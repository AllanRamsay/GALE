import sys, os, re
from useful import *

def getwlist(d=".", ifile="dict.txt", wlist="wlist"):
    if isinstance(wlist, str):
        wlist = os.path.join(d, wlist)
    with safeout(wlist) as write:
        for l in open(os.path.join(d, ifile)):
            write(l.split()[0]+"\n")
