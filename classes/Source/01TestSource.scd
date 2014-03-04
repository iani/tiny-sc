/*
First tests with:
    Source
    MapFunc (MapSet, UnmapSet, BimapSet)

IZ Tue, Mar  4 2014, 16:16 EET
*/

//:
w = Window();
v = Slider();

w.view.layout = HLayout(v);

~source = Source(v);

w.front;

//: Test just if you can connect to the source.  OK.

\test.addNotifier(~source, \value, { | ... args | args.postln; });

//:  Test sound control. OK. 

a = { SinOsc.ar(\freq.kr(400), 0, 0.1) }.play;
a.addNotifier(~source, \value, { | val | a.set(\freq, val * 100 + 400) });

//: Now change the source of ~source

~source.source = 0.
//: start the source

~source.start;
//: stop the source

~source.stop;