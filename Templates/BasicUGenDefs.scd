
SynthTemplates([
whitenoise: SynthDef("whitenoise", { WhiteNoise.ar.ladsrOut }),
graynoise: SynthDef("graynoise", { GrayNoise.ar.ladsrOut }),
sine: SynthDef("sinosc", { SinOsc.ar(\freq.kr(440)).ladsrOut }),
], 'basic')
