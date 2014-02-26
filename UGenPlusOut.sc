/* iz Wed, 26 Feb 2014 08:22:41

Shortcut for sending ugens to output.  Useful for SynthDefs.

Instead of: 
SynthDef("test", { | out = 0 | Out.ar(out, WhiteNoise.ar(0.1)) }).add; 

Write: 
SynthDef("test", { WhiteNoise.ar(0.1).out }).add; 

Todo: Auto-detect if the UGen is ar or kr, and create corresponding ugen.

*/

+ UGen {
	out { | outName = \out, outValue = 0 |
		^Out.ar(outName.perform(\ar, outValue), this)
	}
}