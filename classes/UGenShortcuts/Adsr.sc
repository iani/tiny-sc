/*
ADSR envelope shortcut
 
IZ Tue, 25 Feb 2014 16:23:39 

Not done yet.

*/

Adsr {
	*new { | attackTime = 0.02, decayTime = 0.3, sustainLevel = 0.5, releaseTime = 1, 
		peakLevel = 1, curve = -4, bias = 0, 
		gateName = \gate, gateValue = 1,
		ampName = \amp, ampValue = 0.1,
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
		ampName = \amp, ampValue = 0.1,
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
		ampName = \amp, ampValue = 0.1,
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

+ Node {
	fadeOut { | fadeOut = 0.2 |
		this.set(\timeScale, fadeOut, \gate, 0)
	}
}