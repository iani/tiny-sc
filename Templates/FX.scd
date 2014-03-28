
SynthTemplates([
lpf: SynthDef("lpf", {  LPF.ar(Inp.ar, \freq.kr(2000)).adsrOut; }),
hpf: SynthDef("hpf", {  HPF.ar(Inp.ar, \freq.kr(500)).adsrOut; }),
], 'fx')
