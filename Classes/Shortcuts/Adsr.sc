/*
ADSR envelope shortcut
 
IZ Tue, 25 Feb 2014 16:23:39 

Not done yet.

*/

Adsr {
	*new { | attackTime = 0.02, decayTime = 0.0, sustainLevel = 1, releaseTime = 0.02, 
		peakLevel = 1, curve = -4, bias = 0, 
		gateName = \gate, gateValue = 1,
		ampName = \amp, ampValue = 1,
		doneAction = 2 |
		^ampName.perform(\kr, ampValue) * EnvGen.kr(
			Env.adsr(attackTime, decayTime, sustainLevel, releaseTime, peakLevel, curve, bias),
			gate: gateName.perform(\kr, gateValue),
			levelScale: 1,
			levelBias: 0,
			timeScale: \timeScale.kr(1),
			doneAction: doneAction;
		)
	}
}

Perc {
	*new { | attackTime = 0.02, releaseTime = 1, level = 1, curve = -4, 
		gateName = \gate, gateValue = 1,
		ampName = \amp, ampValue = 1,
		doneAction = 2 |
		^EnvGen.kr(
			Env.perc(attackTime, releaseTime, level, curve),
			gate: gateName.perform(\kr, gateValue),
			levelScale: ampName.perform(\kr, ampValue),
			levelBias: 0,
			timeScale: \timeScale.kr(1),
			doneAction: doneAction;
		)
	}
}

Sine {
	*new { | dur = 1, level = 1, 
		gateName = \gate, gateValue = 1,
		ampName = \amp, ampValue = 1,
		doneAction = 2 |
		^EnvGen.kr(
			Env.sine(dur, level),
			gate: gateName.perform(\kr, gateValue),
			levelScale: ampName.perform(\kr, ampValue),
			levelBias: 0,
			timeScale: \timeScale.kr(1),
			doneAction: doneAction;
		)
	}
}

Inp { 
	*ar { | inputName = \in, channelNum, numChannels = 1 |
		^In.ar(
			inputName.kr(channelNum ?? { Server.default.options.numInputBusChannels }),
			numChannels
		);
	}
	*kr { | inputName = \in, channelNum = 0, numChannels = 1 |
		^In.kr(inputName.kr(channelNum), numChannels);
	}
}

/* Note: 
Use instead of \symbol.ar, which breaks audio linking in SynthTree and Ndef.
*/
+ Symbol {
	in { | channelNum, numChannels = 1 |
		^Inp.ar(this, channelNum, numChannels)
	}
}

/*
+ Node {
	fadeOut { | fadeOut = 0.2 |
		[this, thisMethod.name, "fadeOut:", fadeOut].postln;
		//		this.set(\timeScale, fadeOut, \gate, 0)
		this.release(fadeOut);
	}
}
*/