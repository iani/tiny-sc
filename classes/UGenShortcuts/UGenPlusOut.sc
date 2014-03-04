/* 
Shortcut for sending ugens to output.  Useful for SynthDefs.

Instead of: 
SynthDef("test", { | out = 0 | Out.ar(out, WhiteNoise.ar(0.1)) }).add; 

Write: 
SynthDef("test", { WhiteNoise.ar(0.1).out }).add; 

IZ Wed, 26 Feb 2014 08:22:41

*/

+ UGen {
	out { | outName = \out, outValue = 0 |
		^Out.perform(
			if (this.rate == \control) { \kr } { \ar },
			outName.kr(outValue),
			this
		)
	}

	// TODO: combine envelope and out in one message:
	envOut {
		// plain Env from values - times array
		/* // Draft:
			this.out(... , this * EnvGen.kr(Env(...)))
		*/

	}

	adsrOut {
		/* // draft:
			this.out(... , this * Adsr(...))
		*/
	}

	percOut {
		/* // draft:
			this.out(... , this * Perc(...))
		*/
	}

	sineOut {

	}
}