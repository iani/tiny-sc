
 \default => \test;

[note: [1, 3, 5, 10].prand + Pstutter(10, [0, 7, 9].pseq, inf)] => \test;

[legato: 0.1] => \test;

[\instrument: [\sine, \default, \bassfoundation, \moogbasstone].pseq] => \test;

[0.1 pn: 8, 0.2 pn: 4, 0.05 pn: 12, 0.6].prand *> \dur;

//:

[note: [1, 3, 5, 10].prand + Pstutter(10, [0, 7, 9].pseq, inf) 
+ Pstutter(60, Pbrown(-10, 10, 5, inf) * Pstutter(10, [1, 2, 3].prand))
] => \test;

//:
{ 0.01 exprand: 0.2 } *> \amp;

//:
{ 0.1 exprand: 0.35 }.pfunc *> \legato;

//:

[
Pstutter(
	[1, 2, 4, 8].prand,
	0.1 pn: 8
), 
Pstutter(
	[1, 2, 4, 8].prand,
	0.2 pn: 4
), 
Pstutter(
	[1, 2, 4, 8].prand,
	0.05 pn: 12
), 
Pstutter(
	[1, 2, 4, 8].prand,
	0.6 pn: 1
),
2].prand *> \dur;

//:

0.11 pn: inf *> \dur;