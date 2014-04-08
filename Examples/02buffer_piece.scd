/*
BufferFunc.initBuffers;


bell
bells_back
bells_far
bells_kitchen
bells_later
bells_plus_kitchen
boat
boat_attach
boat_attach_back

*/

a = { SinOsc.ar(440, 0, 0.1) }.play;

a.free;

a release: 5;
\tiny2.free;
//:
{ SinOsc.ar } => \tiny;
//:
{ SinOsc.ar(550) } => \tiny2;
//:
SinOsc.ar(\freq.kr(440));
//:
400 rrand: 1000 => ~freq;
//:
(
{ | i |
	{ Pan2.ar(\buf.playBuf(loop: 1), \pos.kr(0)); } => format("bell%", i).asSymbol.buf(\bell).set(\amp, 0.1)
	.set(\rate, 0.5);
} ! 10;
)
//:
(
{ | i |
	format("bell%", i).asSymbol.set(\rate, i / 10 + 1, 'amp', 0.2);
} ! 10;
)
//:

(
{ | i |
	format("bell%", i).asSymbol.set(\rate, i + 0.1 * 0.4 - 3.3, \amp, 0.1);
} ! 10;
)

//:
~all = { | i | format("bell%", i).asSymbol } ! 10;
~all do: _.set(\amp, 0.00);
~all do: _.set(\pos, 1);
//:
BufferFunc.postBufNames;

//:

{ Pan2.ar(\buf.playBuf(loop: 1)); } => \bell.buf;

\bell.buf(\boat);

\bell.set(\rate, 3);