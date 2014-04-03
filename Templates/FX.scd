
SynthTemplates([
lpf: SynthDef("lpf", {  LPF.ar(Inp.ar, \freq.kr(2000)).ladsrOut; }),
hpf: SynthDef("hpf", {  HPF.ar(Inp.ar, \freq.kr(500)).ladsrOut; }),
], 'fx')
