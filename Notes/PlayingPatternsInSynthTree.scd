/*
Stydying PatternPlayer in order to develop the version for playing
paterns in SynthTree, pbind-like. 
Tue, Apr  1 2014, 21:19 EEST
*/

//: TEST 1: Play a PatternInstrument with a notifier action
//: !!!!!!!!!!!!!!!! OK !!!!!!!!!!!!!!!!

p = [freq: Prand((30..90).midicps, inf)] => 0.02;

\test.addNotifier (p.pattern, \value, { | value |
	var synth;
	synth = Synth ('default', value.params);
	SystemClock.sched (value.dur, { synth.release });
	[value.params, value.dur].postln; });

p.start;

//: TEST 2: Play some events in the PatternSynth of a SynthTree
// !!!!!!!!!!!!!!!! OK
\test.asSynthTree =!> \st;
~pstest = PatternSynth(~st);

~synth = ~pstest.addSynth;
//:
~synth.release;
//: TEST 3: Let the SynthTree create the PatternSynth and put it in its synth
// OK!
p = [freq: (75..90).midicps.prand] => pseq([Pseries(0.05, 0.01, 25), Pseries(0.30, -0.01, 25)]);
p => \newtest;
//: TEST 4: As 3, chaining chucks
[freq: (30 rrand: 75 .. rrand(40, 90)).midicps.prand] 
=> pseq([Pseries(0.01 * rrand(1, 5.0), 0.01, 25), Pseries(0.30, -0.01, 25)])
=> \newtest;

//:
p.start;
//:
p.inspect;
//:
\newtest.asSynthTree.inspect;
//:
a = ~st.synth;
a.addSynth;
a.isPlaying;
a.group;
a.busIndex;

a.set(\in, a.busIndex);

p.start;

//:
b = { }
//:
SynthEvent;
//:
~st.addNotifier(a, \value, { | synthEvent |
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