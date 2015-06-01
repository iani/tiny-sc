
BeatPlayer(\beat1).pattern = 1;

Beat.add(\beat1, \test, { "testing".postln });
BeatPlayer(\beat1).start;

BeatPlayer(\beat1).stop;

//: ================

{ WhiteNoise.ar(0.1) } => \c1;