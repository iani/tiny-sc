

//:
{ 
	SinOsc.ar(440, 0,
		EnvGen.kr(
			Env.adsr(0.02, 0.3, 0.5, 1, 1, -4, 0),
			gate: 1,
			levelScale: 0.1,
			levelBias: 0,
			timeScale: 1,
			doneAction: 2
		)
	)
}.play;

//:

a = { SinOsc.ar(600 rrand: 700, 0, Adsr()) }.play;
//:
a.release(2);
//:
{ LFTri.ar(400 rrand: 500, 0, Perc()) }.play;
//:
{ LFTri.ar(SinOsc.kr(5, 0, 10, 400 rrand: 1500), 0, Perc()).postln }.play;
//:
{ SinOsc.ar(500 rrand: 600, 0, Sine()) }.play;
//:
{ SinOsc.ar(SinOsc.kr(5, 0, 10, 500 rrand: 600), 0, Sine()) }.play;
//:
{ SinOsc.ar(500 rrand: 600, 0, Sine() * SinOsc.kr(5, 0, 0.4, 1)) }.play;
