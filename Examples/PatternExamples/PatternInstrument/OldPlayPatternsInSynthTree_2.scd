/*
More examples of patterns in SynthTree.

Note: To replace the pattern of a single parameter in a synthree chosen by name, use 

pattern -> parameterName => \synthTreeName

For example: 

500 -> \freq => \pattern1
means: 
Set the parameter \freq in synthree named \pattern1 to 500. 

Tue, Apr  1 2014, 21:19 EEST
*/

[freq: (75..90).midicps.prand, dur: pseq([Pseries(0.3, -0.01, 25), Pseries(0.01, 0.01, 25)])]
=> \pattern1;
//: Test pyramid
[freq: (50 rrand: 55 .. rrand(70, 80)).midicps
	.curdle(0.5).scramble.flop(4).flat.pyramid
	.pseq, 
	dur: ((1..8)/16).pyramid.pseq]
=> \pattern1;

//:
[freq: (40..80).midicps.prand] => \pattern1;

//:
[freq: (40..50).midicps.prand, dur: Pstutter(3, Pbrown(0.05, 0.15, 0.05))] => \pattern1;

//:
500 -> \freq => \pattern1;

//:
([500, 600]).prand -> \freq => \pattern1;

//:
[500, 600, 700, 900].prand -> \freq => \pattern1;

//:
[
	([500, 600, 550]).prand(15) * Pstutter(12, [0.5, 0.75, 0.8, 0.9, 1, 1.1].prand), 
	[700, 800, 900].prand(10)
].prand -> \freq => \pattern1;

//:
[freq: (40..80).midicps.prand, amp: { 0.01.exprand(0.1) }.pfunc] => \pattern1;

//:
0.9 *> \dur;

//: Cloud event examples

0.02 *> \dur;
[freq: (40..90).midicps.prand, \instrument: \lpfsaw] => \pattern1;

//:
[freq: Pwhite(
	Pstutter(10, Pbrown(40, 50, 1)), 
	Pstutter(10, Pbrown(45, 90, 1))
).midicps,
instrument: \lpfsaw] => \pattern1;

//:
[freq: Pwhite(
	Pstutter(10, Pbrown(40, 50, 1)), 
	Pstutter(10, Pbrown(45, 90, 1))
).midicps,
instrument:
	[Pstutter(Pbrown(1, 30, 1), \sine.pn(2)), 
		Pstutter(Pbrown(1, 30, 2), \lpfpulse.pn(2))].prand
] => \pattern1;
//:
0.15 *> \dur;
[freq: Pwhite(
	Pstutter(5, Pbrown(40, 50, 1)), 
	Pstutter(5, Pbrown(45, 90, 1))
).midicps,
instrument: Pwrand([\lpfpulse, \sine, \lpfsaw],
	Pstutter(30, Pfunc({ Array.rand(3, 0.0, 1).normalizeSum })), 
	inf)
]=> \pattern1;

//:
0.15 *> \dur;
//:
{ 0.1 exprand: 0.95 }.pfunc *> \legato;
//:
Pwhite(0.05, 0.15, inf) *> \amp;
//:
pser(((0..20)/200).pyramid) *> \amp;

//: 