/*
Stydying PatternPlayer in order to develop the version for playing
paterns in SynthTree, pbind-like. 
Tue, Apr  1 2014, 21:19 EEST
*/

//:
a = (1..10).pseq(inf) => 0.2;
a.addDependant({ | ... args | args.postln; });
a.start;
//:

a = SynthPattern(\default, [freq: Pseq((40..80).midicps, inf)]);
// a.inspect;
//:
b = a.asStream;
// b.inspect;
//:
c = b.next;
c.inspect;
//:
a.inspect;
//:
1.next;

1.asStream;

//:================================================================

a = \default %> [freq: Pseq((40..80).midicps, inf)];

//:

\default %> [freq: Pseq((40..80).midicps, inf)] %> 0.1;

//:

p = [freq: Prand((30..90).midicps, inf)] %> 0.02;

\test.addNotifier (p, \value, { | value |
	var synth;
	synth = Synth (value.instrument, value.params);
	SystemClock.sched (value.dur, { synth.release });
	[value.instrument, value.params, value.dur].postln; });


p.start;
//:

p.valueStream;

//:

p = [freq: Prand((30..90).midicps, inf)] %> 0.02 => \sound;
