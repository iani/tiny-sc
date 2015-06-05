
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
	.choose.arps(\freq.kr (800))
} => \test;
//:
(100..300).prand =>.freq \test;
//:
(1000..3000).prand =>.freq \test;
//:
(30..100).scramble.midicps.pseq =>.freq \test;
//:
(50..100).midicps.pseq =>.freq \test;
//:
0.03 =>.fadeTime \test;

//:
0.05 |> \test;
//:
\test.chuck.source.inspect;

//:
`{
	{
		0.3 * Resonz.arps (Decay.kr( Dust.kr(20, 0.1)) * WhiteNoise.ar, Rand (50, 5000))
    }.play (~target, ~outbus, ~fadeTime).register
} => \test;
//:
`{
	{
	  Resonz.arps (Decay.kr( Impulse.kr(20, 0.1)) * WhiteNoise.ar, ~freq, 3)
    }.play (~target, ~outbus, ~fadeTime).register;
} => \test;
//:
`{
	{
		 SinOsc.arps (~freq, 0, 0.1)
    }.play (~target, ~outbus, ~fadeTime).register
} => \test;
//:
Chuck(\test).inspect;

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

"default" => \test;

\test.play;

\test.sched(0.15);
\test.ft = 0.14;


