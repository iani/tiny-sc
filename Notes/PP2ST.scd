/*
IZ Thu, Apr  3 2014, 10:04 EEST

Building the container for playing patterns in a SynthTree. 
*/


//:

p = [freq: Prand((50..70).midicps, inf)] %> 0.2;

//:
p;
//
//:
\test.addNotifier(p, \value, { | value |
	var synth;
	synth = Synth(\default, value.params);
	SystemClock.sched(value.dur, { synth.release });
});

//:
p.start;

//: OK
b = Bus.audio;
~copier = { Out.ar(0, In.ar(b.index)) }.play;
{ Out.ar(b.index, WhiteNoise.ar(0.1)) }.play;
//: OK!

b = Bus.audio;
~copier = { Inp.ar.ladsrOut }.play(args: [in: b.index, fadeIn: 5.1, amp: 0.1]);
{ Out.ar(b.index, LPF.ar(WhiteNoise.ar, (LFNoise0.kr(10).range(500, 5000)))) }.play;

//:
//: OK Test synth is passed output channel as argument on playing.
b = Bus.audio;
~copier = { Inp.ar.ladsrOut }.play(args: [in: b.index, fadeIn: 3, amp: 0.3]);
c = { LPF.ar(WhiteNoise.ar, (LFNoise0.kr(15).range(500, 5000))).ladsrOut }.play(args: [out: b.index]);


