
{ WhiteNoise.ar(0.1) } +> \test;
{ BrownNoise.ar(0.1) } +> \test;
0.93 +>.fadeTime \test;

{ SinOsc.ar(400, 0, 0.1) } +> \test;
{ SinOsc.ar(\freq.kr (800), 0, 0.1) } +> \test;
{ Silent.ar } +> \test;


\test.release;
\test.free;

600 +>.freq \test;
(100..6000).prand +>.freq \test;

\test.play;
//: Limit on an 2.3 GHz Intel Core i7 MacBook Pro: 500 plays per second
//: Measured CPU load, on sclang: ca 18%, on scsynth: ca 6%
0.002 *> \test;

//:
0.02 *> \test;

//:
{ [LFSaw, SinOsc, LFPulse].choose.ar(\freq.kr (800)) * 0.1 } +> \test;
//:
{
	[LFSaw, SinOsc, LFPulse, LFNoise0, LFNoise1, LFNoise2, Blip]
	.choose.arps(\freq.kr (800))
} +> \test;
//:
(100..300).prand +>.freq \test;
//:
(1000..3000).prand +>.freq \test;
//:
(30..100).scramble.midicps.pseq +>.freq \test;
//:
(50..100).midicps.pseq +>.freq \test;
//:
0.03 +>.fadeTime \test;

//:
0.05 |> \test;
//:
Pwhite(100, 5000, inf) +>.freq \test;
{ SinOsc.ar(\freq.kr (800), 0, 0.1) } +> \test;
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
{ LFSaw.arp(\freq.kr (800)) } +> \test;
//:
{ SinOsc.ar(Rand (400, 800), 0, 0.1) } +> \test;
//:
600 +>.freq \test;

"default" +> \test;




