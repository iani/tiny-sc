/*
Stydying PatternPlayer in order to develop the version for playing
paterns in SynthTree, pbind-like. 
Tue, Apr  1 2014, 21:19 EEST
*/

//: Let the SynthTree create the PatternSynth and put it in its synth
p = [freq: (75..90).midicps.prand] => pseq([Pseries(0.3, -0.01, 25), Pseries(0.01, 0.01, 25)]);
p => \pattern1;

//: As above, chaining chucks
[freq: (30 rrand: 75 .. rrand(40, 90)).midicps.prand] 
=> ([Pseries(0.30, -0.01, 25), Pseries(0.01 * rrand(1, 5.0), 0.01, 25)]).pseq
=> \pattern1;

//: Test pyramid
[freq: (50 rrand: 55 .. rrand(70, 80)).midicps
	.curdle(0.5).scramble.flop(4).flat.pyramid
	.pseq]
=> ((1..8)/16).pyramid.pseq
=> \pattern1;

//:
[freq: (40..80).midicps.prand] => 0.1 => \pattern1;

//:
[freq: (40..50).midicps.prand] => Pstutter(3, Pbrown(0.05, 0.15, 0.05)) => \pattern1;

//:
\freq->500 => \pattern1;

//:
\freq->([500, 600]).prand => \pattern1;

//:
\freq->[500, 600, 700, 900].prand => \pattern1;

//:
\freq->[
	([500, 600, 550]).prand(15) * Pstutter(12, [0.5, 0.75, 0.8, 0.9, 1, 1.1].prand), 
	[700, 800, 900].prand(10)
].prand => \pattern1;

//:
[freq: (40..80).midicps.prand, amp: { 0.01.exprand(0.1) }.pfunc] => \pattern1;

//:
0.1 =!> \dur;

//: Cloud event examples

0.02 =!> \dur;
[freq: (40..90).midicps.prand] => `\lpfsaw => \pattern1;

//:
[freq: Pwhite(
	Pstutter(10, Pbrown(40, 50, 1)), 
	Pstutter(10, Pbrown(45, 90, 1))
).midicps ] => `\lpfsaw => \pattern1;

//:
[freq: Pwhite(
	Pstutter(10, Pbrown(40, 50, 1)), 
	Pstutter(10, Pbrown(45, 90, 1))
).midicps ] =>.i [
	Pstutter(Pbrown(1, 30, 1), \sine.pn(2)), 
	Pstutter(Pbrown(1, 30, 2), \lpfpulse.pn(2))].prand => \pattern1;
//:
0.02 =!> \dur;
[freq: Pwhite(
	Pstutter(5, Pbrown(40, 50, 1)), 
	Pstutter(5, Pbrown(45, 90, 1))
).midicps ] 
=>.i Pwrand([\lpfpulse, \sinosc, \lpfsaw],
	Pstutter(30, Pfunc({ Array.rand(3, 0.0, 1).normalizeSum })), 
	inf)
=> \pattern1;

//:
0.1 =!> \dur;
//:
~st.legato = Pwhite(0.2, 3.2, inf);
//:
\amp -> Pwhite(0.01, 0.1, inf) => ~st;
//:
\amp -> pser(((0..200)/2000).pyramid) => ~st;
