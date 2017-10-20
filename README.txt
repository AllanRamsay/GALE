I've tried to set it up so that everything can be done by importing
HTK.py and then running various commands at the prompt. Depending on
whether you imported it by doing

>>> import HTK

or 

>>> from HTK import *

you may need to stick HTK. in front of the commands below. I generally
run Python programs by opening them in Aquamacs and doing ^C^C to
import them. With the way that I have Aquamacs set, this is equivalent
to doing "from HTK import *", but you may be using a different
environment, or even just the command line interpreter, and even if
you are using Aquamacs I know that some versions do "import HTK" when
you do ^C^C. 

Preparing the raw GALE data
---------------------------
The GALE data consists of set of parallel files, in
gale_p3_ara_bn_transcripts_p1 and gale_p3_bn_speech_p1, with names
like SYRIANTV_NEWS25_ARB_20070403_162800.qrtr.tdf and
SYRIANTV_NEWS25_ARB_20070403_162800.wav. These are quite large files,
where the .tdf files contain time stamps. So the very first thing we
have to do is split them into sub-files. We do this by doing

>>> segments.getPrompts(src=os.path.join(TDF, TESTPROMPT), dest="TEMP", promptsfile="originalprompts.segments")

Read <N> prompts from the folder <prompts> and copy the corresponding
bits of wav files from <wav> to <dest>/wav

src is where the .wav files and originalprompts are stored. We copy
them to EXPT (unless we've already done so) and run the HTK with N
prompts as training data. rounds is how many HERests we do at each
stage, localtests is how many of the test prompts we use for
intermediate testing (HVite with a big grammar can be slow, and we
don't really care what the intermediate scores are, just how they vary
we go along)

I'm using !ENTER and !EXIT instead of SENT-START and SENT-END as
boundary markers, since it seems that this will help when we want to

>>> segments.getPrompts(src=os.path.join(TDF, TESTPROMPT), dest="TEMP", promptsfile="originalprompts.segments") do bigram experiments.

(the default values of the various arguments are as in this call, so

>>> segments.getPrompts()

does exactly the same thing)

I've set it up so that you can run MADAMIRA from inside Python. You
might want to take advantage of this, because it means that we can run
the entire script from Python, or after reading the next bit you might
think it's not worth the bother.

The problem is to do with knowing where everything is. You might be in
one directory, MADAMIRA might be in another, and the data you want to
work with might be in a third. You can't easily execute MADAMIRA from
a directory other than the one where it lives, because it wants to
write to the log file, and I can't see any way of specifying a
location for that. So you have to tell the call of runmadamira where
MADAMIRA itself is from here and where the data you want is *from
there*. And since I've put the definition inside segments.py, you also
have to put that into the prefix for the call.

>>> segments.runmadamira(madamiradir="MADAMIRA-release-20170403-2.1", src="../TEMP", dest="../TEMP")

(I've set the defaults as in the call above, so if you have everything
where I do then just segments.runmadamira() will work)

You can include running MADAMIRA when you call getSegments -- makes a
reasonable amount of sense to do it then, because we're almost certain
to want to have it applied to any set of segments that we extract from
GALE. If you do 

>>> segments.getPrompts(src=os.path.join(TDF, TESTPROMPT), dest="TEMP", promptsfile="originalprompts.segments", ranMadamira=True)

then it will run MADAMIRA on TEMP/originalprompts.segments. But given
that you want to do experiments with MADAMIRA with different versions
of the SAMA dictionary, you may need to do the explicit calls as well.

Running the HTK
---------------
Running the HTK is now packaged up inside the class EXPERIMENT (upper
case). The function experiment (lower case) runs a series of
experiments, as in

>>> l = experiment(src="TEMP", dest="MADAEXPT", mada=["baseline", "sama31", "madamira31"], prolog=[0], forcedAlignment=["Y"], configs=[useconfig39], dicts=["fixed", "multi"], stages=["no", "yes"], localtests=3, N=sys.maxint)

I've renamed the bits that are used in fixPhones, as we discussed on
Friday. 31 is "all the bits that fixPhones looks at"; and I renamed
the one that does the baseline experiment to "baseline". I think
everything else is as it was. N is the maximum of prompts to be
considered. If you set it too low then training crashes. At the end l
should be a set of EXPERIMENTS with scores.

Checking the output of MADAMIRA and SAMA
----------------------------------------
You wanted to be able to assess the effects of making changes to the
SAMA dictionary on MADAMIRA and on SAMA itself. To do that, run
MADAMIRA to produce a file wih a name like
"TEMP/originalprompts.segments.mada" and then do

>>> import mada
>>> mada.mada2csv(ifile="TEMP/originalprompts.segments.mada", out="TEMP/mada.csv")

That will put the output of MADAMIRA and SAMA in TEMP/mada.csv

Then

>>> x = mada.getunrecog("TEMP/mada.csv") 

will print something like

1234 tokens out of 5362 are unrecognised by SAMA
394 distinct words out of 1769 are unrecognised by SAMA
171 tokens out of 5362 are unrecognised by MADAMIRA
121 distinct words out of 1769 are unrecognised by MADAMIRA

x will be a tuple consisting of four dictionaries -- words
unrecognised by SAMA, words recognised by SAMA, words unrecognised by
MADAMIRA, words recognised MADAMIRA. From what we have above, it looks
as though at least some of the words that SAMA fails to recognise are
actually quite high frequency. We can improve SAMA's performance by
getting it to look at variations of words that start with "A..." in
the GALE data but that ought really to start "I..." or "W..."; but I'm
not measuring that here.
