
{ WhiteNoise.ar(0.1) } => \test;
{ BrownNoise.ar(0.1) } => \test;
0.93 =>.fadeTime \test;
// 0 =>.fadeTime \test;
{ SinOsc.ar(400, 0, 0.1) } => \test;
{ SinOsc.ar(\freq.kr (800), 0, 0.1) } => \test;
{ Silent.ar } => \test;


\test.release;
\test.free;


{ BrownNoise.ar(0.1) } || \test;

600 *>.freq \test;
(400..600).prand *>.freq \test;

\test.play;
//:
{
	loop {
		\test.play;
		1.wait;
	}
}.fork

//:
{
	loop {
		\test.play;
		0.1.wait;
	}
}.fork

//:
{
	loop {
		\test.play;
		0.015.wait;
	}
}.fork

//:
{ [LFSaw, SinOsc, LFPulse].choose.ar(\freq.kr (800)) * 0.1 } => \test;
//:
{
	[LFSaw, SinOsc, LFPulse, LFNoise0, LFNoise1, LFNoise2, Blip]
	.choose.ar(\freq.kr (800)) * 0.1
} => \test;
//:
(100..300).prand =>.freq \test;
//:
(100..3000).prand =>.freq \test;
//:
(30..100).scramble.midicps.pseq =>.freq \test;
//:
(50..100).midicps.pseq =>.freq \test;
//:
1 =>.fadeTime \test;

//:

// { "Hello".postln; } => \test;
{ "Hello".postln; } &> \test;
//:
{
	currentEnvironment.postln;
	{
		0.3 * Resonz.ar (Decay.kr( Dust.kr(20, 0.1)) * WhiteNoise.ar, Rand (50, 5000))
    }.play (~target, ~outbus, ~fadeTime)
} &> \test;
//:
{
	{
	  0.1 *	Resonz.ar (Decay.kr( Impulse.kr(20, 0.1)) * WhiteNoise.ar, ~freq, 3)
    }.play (~target, ~outbus, ~fadeTime)
} &> \test;
//:
{
	{
		 SinOsc.ar (~freq, 0, 0.1)
    }.play (~target, ~outbus, ~fadeTime)
} &> \test;
//:
Chuck(\test).inspect;

//: linking

\a @> \b;
\b @> \c;
\b @> \a;
\c @> \a;


\b.chuck.readers;
\a.chuck.readers;

\a.chuck.process.directReaders;



\b.chuck.process.args;

\a.chuck.process.args[\out].inspect;

\a.chuck.process.args[\out].readersTree;

// .readersTree;
\a.chuck.readers;

[1] includes: 1;
//:
{ SinOsc.ar(\freq.kr (800), 0, 0.1) } => \test;
//:
{ SinOsc.ar(Rand (400, 800), 0, 0.1) } => \test;
//:
600 =>.freq \test;

\testet =>.freq \test;

BusLink() =>.freq \test;

\test.chuck.process.args;

"default" => \test;

\test.play;

\test.sched(0.15);
\test.ft = 0.14;

