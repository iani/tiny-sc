/* 
Shortcut for sending ugens to output.  Useful for SynthDefs.

Instead of: 
SynthDef("test", { | out = 0 | Out.ar(out, WhiteNoise.ar(0.1)) }).add; 

Write: 
SynthDef("test", { WhiteNoise.ar(0.1).out }).add; 

To add envelopes: 
SynthDef("test", { WhiteNoise.ar(0.1).ladsrOut }).add; 
a = Synth("test");
a release: 0.4;

SynthDef("test2", { WhiteNoise.ar(Adsr()).out }).add; 
b = Synth("test2");
b.set(\timeScale, 1, \gate, 0);

SynthDef("test3", { WhiteNoise.ar.ladsrOut }).add; 
c = Synth("test3", [amp: 0.02]);
c.set(\out, 1);
c.fadeOut(5);


x = { WhiteNoise.ar(Adsr()).out }.play;
x.fadeOut(5);
x.set(\out, 1);

IZ Wed, 26 Feb 2014 08:22:41

{ WhiteNoise.ar }.xplay;

{ WhiteNoise.ar } => \test;



*/

+ UGen {
	out { | outName = \out, outValue = 0 |
		^Out.perform(
			if (this.rate == \control) { \kr } { \ar },
			outName.kr(outValue),
			this
		)
	}

	/* Fix for setting the fade-in time reliably. */
	ladsrOut { | outName = \out, outValue = 0,
		attackTime = 0.02, decayTime = 0.0, sustainLevel = 1, releaseTime = 0.02, 
		peakLevel = 1, curve = -4, bias = 0,
		gateName = \gate, gateValue = 1,
		ampName = \amp, ampValue = 1, doneAction = 2, fadeIn = 0 |
		^(
			Line.kr(0, 1, \fadeIn.kr(fadeIn)) *
			this *
			Adsr(attackTime, decayTime, sustainLevel, releaseTime, 
				peakLevel, curve, bias, gateName, gateValue,
				ampName, ampValue, doneAction
			)
		).out(outName, outValue);
	}

	adsrOut { | outName = \out, outValue = 0,
		attackTime = 0.02, decayTime = 0.0, sustainLevel = 1, releaseTime = 0.02, 
		peakLevel = 1, curve = -4, bias = 0,
		gateName = \gate, gateValue = 1,
		ampName = \amp, ampValue = 1, doneAction = 2 |
		^(
			this * Adsr(attackTime, decayTime, sustainLevel, releaseTime, 
				peakLevel, curve, bias, gateName, gateValue,
				ampName, ampValue, doneAction
			)
		).out(outName, outValue);
	}

	// TODO: combine envelope and out in one message:
	envOut {
		// plain Env from values - times array
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

+ Array {
	out { | outName = \out, outValue = 0 |
		^Out.perform(
			if (this.rate == \control) { \kr } { \ar },
			outName.kr(outValue),
			this
		)
	}

	/* Fix for setting the fade-in time reliably. */
	ladsrOut { | outName = \out, outValue = 0,
		attackTime = 0.02, decayTime = 0.0, sustainLevel = 1, releaseTime = 0.02, 
		peakLevel = 1, curve = -4, bias = 0,
		gateName = \gate, gateValue = 1,
		ampName = \amp, ampValue = 1, doneAction = 2, fadeIn = 0 |
		^(
			Line.kr(0, 1, \fadeIn.kr(fadeIn)) *
			this *
			Adsr(attackTime, decayTime, sustainLevel, releaseTime, 
				peakLevel, curve, bias, gateName, gateValue,
				ampName, ampValue, doneAction
			)
		).out(outName, outValue);
	}

	/* Note:  Setting timeScale of Adsr seems to break the speed
		or final level of the amplitude.  Using Line instead. 
		See ladrsOut. 
	*/
	adsrOut { | outName = \out, outValue = 0,
		attackTime = 0.02, decayTime = 0.0, sustainLevel = 1, releaseTime = 0.02, 
		peakLevel = 1, curve = -4, bias = 0,
		gateName = \gate, gateValue = 1,
		ampName = \amp, ampValue = 1, doneAction = 2 |
		^(
			this * Adsr(attackTime, decayTime, sustainLevel, releaseTime, 
				peakLevel, curve, bias, gateName, gateValue,
				ampName, ampValue, doneAction
			)
		).out(outName, outValue);
	}

	// TODO: complete these
	envOut {
		// plain Env from values - times array
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

+ SimpleNumber {
	ladsrOut { | outName = \out, outValue = 0,
		attackTime = 0.02, decayTime = 0.0, sustainLevel = 1, releaseTime = 0.02, 
		peakLevel = 1, curve = -4, bias = 0,
		gateName = \gate, gateValue = 1,
		ampName = \amp, ampValue = 1, doneAction = 2, fadeIn = 0 |
		^Silent.ar.ladsrOut(
			outName, outValue, attackTime, decayTime, sustainLevel, releaseTime, 
			peakLevel, curve, bias, gateName, gateValue, ampName, ampValue, doneAction, fadeIn
		);
	}
}