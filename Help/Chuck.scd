
{ WhiteNoise.ar(0.1) } => \test;
{ BrownNoise.ar(0.1) } => \test;
0.93 =>.fadeTime \test;
// 0 =>.fadeTime \test;
{ SinOsc.ar(400, 0, 0.1) } => \test;
{ SinOsc.ar(\freq.kr (800), 0, 0.1) } => \test;
{ Silent.ar } => \test;


\test.release;
\test.free;

600 =>.freq \test;
(400..600).prand =>.freq \test;

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
		0.3 * Resonz.arp (Decay.kr( Dust.kr(20, 0.1)) * WhiteNoise.ar, Rand (50, 5000))
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

\a => \b;
{ Resonz.arps(In.ar(\in.kr), LFNoise0.kr(30).range(500, 5000)) } => \b;
{ WhiteNoise.arp } => \a;

\a.chuck.args.inspect;
//:



//:
Server.default.scope;
//:

\b => \c;
\b => \a;
\c => \a;


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
Pwhite(100, 5000, inf) =>.freq \test;
{ SinOsc.ar(\freq.kr (800), 0, 0.1) } => \test;
0.1 |> \test;
//:
0.01 |> \test;
//:
1 |> \test;
//:
[0.04 ! 10, 0.15 ! 3].flat.pseq |> \test;
//:
0.12 |> \test;
//:
Pseq([Pstutter(5, Pfuncn({ 0.05 exprand: 0.35 }, 1)), 0.7], inf) |> \test;
//:
0.5 |> \test;
//:
0.01 |> \test;
//:
Pn(0.1, 5) |> \test;
//:
Pseq([Pstutter(15, Pfuncn({ 0.04 exprand: 0.4 }, 1)), 2], inf) |> \test;
//:
{ LFSaw.arp(\freq.kr (800)) } => \test;
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

\test.chuck.process.args;

//:
Synth("default").isPlaying;
//:
a = Synth("default").addNotifier(\test, \x, {});
a.isPlaying;
//:
\x |> \y;
0.15 |>\y;
//:
[0.15, 0.3].pseq |>\y;
//:
[0.15, 0.15, 0.15, 0.3].pseq |>\y;
//:
\x |>.xoxxooxxxxxoooxxxxooooxxxxxooooooooxoooooooo \y;
{ WhiteNoise.arps.perc(0.1) } ==> \x;

//:
\z |> \x;

{ SinOsc.arps(Rand(400, 800) )} ==> \z;

//:
\z |> \x;

{ SinOsc.arps(Rand(60, 100).midicps )} ==> \z;

//:
\z |>.xox___xoooxoxx \y;
//:
\z |>.xox___xoxo \x;
//:
{ LFSaw.arps(Rand(400, 800)).perc(0.1) } ==> \z;

//:

{ LFSaw.arps(Rand(400, 8000).perc(0.5)).perc(0.5) }  ==> \z;

//:
{ SinOsc.arps(Rand(400, 8000).perc(0.5)).perc(0.5) }  ==> \z;
//:
{ SinOsc.arp({ Rand(400, 8000) }.dup.perc(0.5)).perc(0.5) }  ==> \z;
//:
{ LFSaw.arp({ Rand(400, 8000) }.dup.perc(0.5)).perc(0.5) }  ==> \z;


//:
1 |> \a;

