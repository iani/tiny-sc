/*
IZ Thu, Apr  3 2014, 10:04 EEST

+Play with:  

H-M-space:
H-M-n:
H-M-p: 
H-space:

*/

//:
WhiteNoise.ar 
//:
GrayNoise.ar
//:
PinkNoise.ar 
//:
SinOsc.ar(550)
//:
LPF.ar(LFPulse.ar(30.rrand (90).midicps), 500)
//:
LPF.ar(LFPulse.ar(LFNoise0.kr(6).range(20, 40).midicps), LFNoise1.kr (12!2).range(500, 2000))
//:
LPF.ar(LFSaw.ar(LFNoise0.kr(8).range(30, 60).midicps), LFNoise1.kr (12!2).range(500, 800))
//:
Mix (SinOsc.ar(50.rrand (70) * (1..7), 0, 1/7) )
//:
Mix (SinOsc.ar(LFNoise0.kr (9).range (50, 70).midicps * (1..7), 0, 0.2 / (1..7)) )
//:
Mix (SinOsc.ar(LFNoise0.kr (10).range (50, 70).midicps * [1, 3, 5], 0, 0.2 / (1..3)) )
//:
Mix (SinOsc.ar(LFNoise0.kr (11).range (50, 70).midicps * [1, 3, 5, 7], 0, 0.2 / (1..4)) )
//:
Mix (SinOsc.ar(LFNoise0.kr (11).range (30, 50).midicps * [1, 3, 5, 7], 0, 0.2 / (1..4)) )