/*
IZ Thu, Apr  3 2014, 10:04 EEST

Play with:  

H-M-space:
H-M-n:
H-M-p: 
H-space:

*/

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
LPF.ar(LFPulse.ar(LFNoise2.kr (6).range (100, 1000)), LFNoise1.kr (7).range (500, 2000))