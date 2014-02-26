/* iz Wed, 26 Feb 2014 08:27:08
Testing out shortcut on SynthDefs
*/

//:
SynthDef("percnoise", { WhiteNoise.ar(Perc()).out }).add;
//:

Synth("percnoise");

//:
r = {
	loop {
		Synth("percnoise", [\out, 2.rand, \amp, 0.01 rrand: 0.2]);
		0.01.exprand(1).wait;
	}
}.fork

//:
r.stop;

//:
q = {
	loop {
		Synth("percnoise", [\out, 2.rand, \amp, [0.05, 0.1].choose]);
		[0.1 ! 4, 0.2 ! 6, 0.4 ! 2, 0.8].flat.choose.wait;
	}
}.fork

//:
q.stop;