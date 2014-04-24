
SynthTemplates([
whitenoise: SynthDef("whitenoise", { WhiteNoise.ar.ladsrOut }),
graynoise: SynthDef("graynoise", { GrayNoise.ar.ladsrOut }),
sine: SynthDef("sine", { SinOsc.ar(\freq.kr(440)).ladsrOut }),
saw: SynthDef("saw", { LFSaw.ar(\freq.kr(440)).ladsrOut }),
pulse: SynthDef("pulse", { LFPulse.ar(\freq.kr(440)).ladsrOut }),
lpfsaw: SynthDef("lpfsaw", { 
	LPF.ar(LFSaw.ar(\freq.kr(440)), \filterFreq.kr(1000)).ladsrOut 
}),
lpfsawr: SynthDef("lpfsawr", { | freq = 440 |
	LPF.ar(LFSaw.ar(freq), \ratio.kr(2) * freq).ladsrOut 
}),
lpfpulse: SynthDef("lpfpulse", { 
	LPF.ar(LFPulse.ar(\freq.kr(440)), \filterFreq.kr(1000)).ladsrOut 
}),
lpfpulser: SynthDef("lpfpulser", { | freq = 440 |
	LPF.ar(LFPulse.ar(freq), \ratio.kr(2) * freq).ladsrOut 
})
], 'basic', 'simple_instruments')
