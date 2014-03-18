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

(
{ | i |
	{ Pan2.ar(\buf.playBuf(loop: 1)); } => format("bell%", i).asSymbol.buf(\bell).set(\amp, 0.005)
	.set(\rate, 0.1);
} ! 10;
)

//:

(
{ | i |
	format("bell%", i).asSymbol.set(\rate, i / 39.3 + 0.0);
} ! 10;
)



//:

~all = { | i | format("bell%", i).asSymbol } ! 10;

~all do: _.set(\amp, 0.01);

~all do: _.set(\pan, 0.5);


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