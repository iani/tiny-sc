/*
ADSR envelope shortcut
 
IZ Tue, 25 Feb 2014 16:23:39 

Not done yet.

*/

Adsr {
	*new { | attackTime = 0.01, decayTime = 0.3, sustainLevel = 0.5, releaseTime = 1, 
		peakLevel = 1, curve = -4, bias = 0, 
		gateName = \gate, gateValue = 1,
		ampName = \amp, ampValue = 0.1,
		doneAction = 2 |
		^EnvGen.kr(
			Env.adsr(attackTime, decayTime, sustainLevel, releaseTime, peakLevel, curve, bias),
			gate: Control.kr(gateName, gateValue), 
			levelScale: Control.kr(ampName, ampValue), 
			levelBias: 0, 
			timeScale: 1, doneAction: doneAction
		)
	}
}