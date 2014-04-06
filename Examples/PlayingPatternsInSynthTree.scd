/*
Stydying PatternPlayer in order to develop the version for playing
paterns in SynthTree, pbind-like. 
Tue, Apr  1 2014, 21:19 EEST
*/

//: TEST 3: Let the SynthTree create the PatternSynth and put it in its synth
// OK!
p = [freq: (75..90).midicps.prand] => pseq([Pseries(0.05, 0.01, 25), Pseries(0.30, -0.01, 25)]);
p => \newtest;
//: TEST 4: As 3, chaining chucks
// OK!
[freq: (30 rrand: 75 .. rrand(40, 90)).midicps.prand] 
=> pseq([Pseries(0.01 * rrand(1, 5.0), 0.01, 25), Pseries(0.30, -0.01, 25)])
=> \newtest;

//: Test pyramid
[freq: (50 rrand: 55 .. rrand(70, 80)).midicps
	.curdle(0.5).scramble.flop(4).flat.pyramid
	.pseq]
=> ((1..8)/16).pyramid.pseq
=> \newtest;
//:
[freq: (40..80).midicps.prand] => 0.1 => \newtest;
//:
[freq: (40..50).midicps.prand] => Pstutter(3, Pbrown(0.05, 0.15, 0.05)) => \newtest;
//:
\freq->500 => \newtest;
//:
\freq->([500, 600]).prand => \newtest;
//:
\freq->[500, 600, 700, 900].prand => \newtest;
//:
\freq->[
	([500, 600, 550]).prand(15) * Pstutter(12, [0.5, 0.75, 0.8, 0.9, 1, 1.1].prand), 
	[700, 800, 900].prand(10)
].prand => \newtest;