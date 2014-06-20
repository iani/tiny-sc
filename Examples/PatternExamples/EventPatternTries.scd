/*
EventPattern plays both inside sequential patterns (Pseq etc), 
and inside parallel patterns (Ppar), and on its own (like Pbind).

IZ Mon, Apr 21 2014, 17:14 EEST

Examples:

*/


a = EventPattern((degree: [1, 2, 3].pseq(2), dur: 0.1)).play;

//:
b = Pn(EventPattern((degree: [1, 2, 3].pseq(2), dur: 0.1)), 2).play;

//:
(
b = Pseq(
	[
		EventPattern((degree: [1, 2, [3, 1, -1, -4, -1, 1, 3, 5, 7, 9, 11, 13]].pseq(1), 
			dur: 2, strum: 0.05)),
		EventPattern((degree: (-2..15).pseq(1), dur: 0.05)),
	]
).play;
)
//:
(
b = Ppar(
	[EventPattern((degree: [20, 21, 25,
		{ (0, 2 .. 14).scramble + (0 rrand: 4) }.pfuncn(1)].prand(15), 
		dur: 1, strum: 0.05)),

		Pseq([
			EventPattern((degree: (-20..25).pseq(1), dur: 0.15)),
			EventPattern((note: Pseq([20, 21], 20), dur: 0.05)),
			EventPattern((degree: Pn((0, 2 .. 12), 1), dur: 0.3))
		]),
	]

).play;
)