

a = MixBus();

b = { WhiteNoise.ar(\amp.kr(0.1)).out; }.play;

b.set(\out, a.index);

a.level = 0.2;

a.fadeOut(5);

//:

b = { SinOsc.ar(440, 0, \amp.kr(0.1)).out; }.play;

b => \default;

c = { LFTri.ar(550, 0, 0.1).out }.play;

c => \default;

\default.mixBus.fadeOut(5);

