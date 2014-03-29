
SynthTemplates([
whitenoise: SynthDef("whitenoise", { WhiteNoise.ar.adsrOut }),
graynoise: SynthDef("graynoise", { GrayNoise.ar.adsrOut }),
sine: SynthDef("sinosc", { SinOsc.ar(\freq.kr(440)).adsrOut }),
], 'basic')
