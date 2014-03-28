
SynthTemplates([
whitenoise: { WhiteNoise.ar },
graynoise: { GrayNoise.ar },
sine: { SinOsc.ar(\freq.kr(440)) },
], 'basic')
