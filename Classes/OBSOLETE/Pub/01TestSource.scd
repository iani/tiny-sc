/*
First tests with:
    Pub
    MapFunc (MapSet, UnmapSet, BimapSet)

IZ Tue, Mar  4 2014, 16:16 EET

Tests passed: Tue, Mar  4 2014, 22:23 EET
*/

//:
~source = Pub();
w = Window("test", Rect(1250, 0, 150, 200));
v = Slider();
w.view.layout = HLayout(v);

~source.source = v;

w.front;

//: Test just if you can connect to the source.  OK.

\test.addNotifier(~source, \value, { | ... args | args.postln; });

//:  Test sound control. OK. 

a = { SinOsc.ar(\freq.kr(400), 0, 0.1) }.play;
a.addNotifier(~source, \value, { | val | a.set(\freq, val * 100 + 400) });

//: Now change the source of ~source

~source.source = 0.090;
//: start the source

~source.start;
//: stop the source

~source.stop;

//: Array
~source.source = [0.1, 0.5];
//: Array

~source.source = (0..20).scramble.pyramid / 10;

~source.source = (0..10).scramble.pyramid(5) / 10;

~source.source = (0..10);

//: New adapter
a.addNotifier(~source, \value, { | val | a.set(\freq,(val * 2 + 50).midicps.postln) });
