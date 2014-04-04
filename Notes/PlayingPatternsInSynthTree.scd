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

a = SynthPattern([freq: Pseq((40..80).midicps, inf)]);
// a.inspect;
//:
b = a.asStream;
// b.inspect;
//:
c = b.next(0.1);
c.inspect;
//:
a.inspect;

//:================================================================

a = [freq: Pseq((40..80).midicps, inf)] %> 0.1;

//:

p = [freq: Prand((30..90).midicps, inf)] %> 0.02;

\test.addNotifier (p, \value, { | value |
	var synth;
	synth = Synth ('default', value.params);
	SystemClock.sched (value.dur, { synth.release });
	[value.params, value.dur].postln; });

p.start;
//:

p.valueStream;

//: What should work:
[freq: Prand((30..90).midicps, inf)] %> 0.02 => \sound;
//:
\test.asSynthTree =!> \st;
~pstest = PatternSynth(~st);
//:
~st.addNotifier(~st, \value, { | synthEvent |
	var synth;
	synth = ~pstest.addSynth(\default, synthEvent.params);
	SystemClock.sched(synthEvent.dur, { synth.release });
});
//:
~pstest.removeNotifier(~pattern, \value);
//:
~pstest.busIndex;
//:

~synth = Synth(\default, [\out, 16]);
//:
~synth.release;
//:
~synth.set(\out, 4)

//:
SynthDescLib.global.browse;
//:
~synth = ~pstest.addSynth;
//:
~synth.release;
//:
~pattern = [freq: Prand((30..90).midicps, inf)] %> 0.02;
//:
~pattern.start;
//:
~pattern addDependant: { | a, b, c | c.params.postln; };
~pattern.release;