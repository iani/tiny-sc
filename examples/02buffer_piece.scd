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

//:
a = { | freq = 400 | SinOsc.ar(freq, 0, 0.1); }.play;
//:
a = { SinOsc.ar(\freq.kr(440), 0, 0.1) }.play;
//:
{ SinOsc.ar } => \tiny;
//:
SinOsc.ar(\freq.kr(440));
//:
a.set(\freq, 880);



//:
(
{ | i |
	{ Pan2.ar(\buf.playBuf(loop: 1), \pos.kr(0)); } => format("bell%", i).asSymbol.buf(\bell).set(\amp, 0.01)
	.set(\rate, 0.5);
} ! 10;
)

//:

(
{ | i |
	format("bell%", i).asSymbol.set(\rate, i + 0.1 * 0.4 - 14.3);
} ! 10;
)



//:

~all = { | i | format("bell%", i).asSymbol } ! 10;

~all do: _.set(\amp, 0.6);

~all do: _.set(\pos, 1);


//:

(
{ | i |
	format("bell%", i).asSymbol.set(\rate, i / 10 + 1);
} ! 10;
)

\bell0.set(\


//:
BufferFunc.postBufNames;

//:

{ Pan2.ar(\buf.playBuf(loop: 1)); } => \bell.buf;

\bell.buf(\boat);

\bell.set(\rate, 3);