/*
Edef is compatible with Ndef
Wed, Apr 30 2014, 20:04 EEST

Examples:
*/

//: Edef inheritance of changes in Ndef OK: 


Edef(\n, (dur: 1, degree: 1)) =>@ \test;

Edef(\n) =>@ \test2;

\n =< (dur: 1, degree: [1, 2, 3, 2].prand);

\n =< (dur: 1, degree: [1, 2, 3, 2].pseq);

\n =< (dur: 0.1);

\n =< (legato: 0.5);

\n =< (instrument: \bassfoundation);

\n =< (degree: Pbrown(-10, 20, 5, inf));

{ WhiteNoise.ar(0.01) } =>@ \test;

(degree: 1, dur: 0.1) =>@ \test;

Pbind(\degree, 1, \dur, 0.1) =>@ \test;

Pbind(\degree, [0, 1].pseq, \dur, 1.1) =>@ \test;

[degree: (0..7).pseq, dur: 0.1] =>@ \test;

//

Pdef(\z, Pbind(\degree, [10, 11].pseq, \dur, 0.05));

Pdef(\z) =>@ \test;



////////////////////////////////////////////////////////////////


// EventPattern is compatible with Pdef:

Pdef(\y, EventPattern((dur: 0.1, degree: [1, 2].pseq)));

Pdef(\y).play;


//: but also with Ndef: 

EventPattern((dur: 0.1, degree: [1, 2].pseq)) =>@ \compat;

//:

Edef(\x, (degree: (0..10).pseq)).play;




//:
Ndef(\compat).inspect;
//:

Edef(\x, (dur: 0.1, degree: [1, 2].pseq)).play;

//: Edef also comaptible with Pdef: 
Edef(\x, (dur: 2, degree: [0, 1, 2, 3, 4, 5, 6, 7].pseq)) =>@ \test;
Edef(\x) =>@ \test2;
Edef(\x) =>@ \test3;


a = Edef(\x, (dur: 1, degree: [0, 1, 2, 3, 4, 5, 6, 7].pseq)).asEventStreamPlayer.play;
b = Edef(\x, (dur: 1, degree: [0, 1, 2, 3, 4, 5, 6, 7].pseq)).asEventStreamPlayer.play;


//:
a = EventPattern((dur: 2, degree: (0..7).pseq));
b = a.play;
c = a.play;

b.stop;

c.stop;

b.start;

c.start;
//:
a.originalStream.event === b.originalStream.event;

//:
a.originalStream === b.originalStream;
//:
a === b;

a.event;

a.inspect;


\x =< (degree: [1, 2, 3, 1].pseq);

\x =< (dur: 1);

Ndef(\test).inspect;


Ndef(\test).clear;

Ndef(\test).play;

Pdef(\x, Pbind(\note, Pbrown(0, 6, 1, inf), \dur, 0.1));
Pdef(\x).quant = 2;
Pdef(\x).play;


Pbindf(Pdef(\x), \dur, 0.03);
Pbindf(Pdef(\x), \dur, 0.1, \ctranspose, 15).play;
Pbindf(Pdef(\x), \dur, 0.3, \ctranspose, 2).play;
// now change the definition
Pdef(\x, Pbind(\note, Pseq([0, 3, 5, 7, 9, 11], inf)));
Pdef(\x, Pbind(\freq, Pseq([1000, 1923, 245.2, 1718] / 1.2 + 0.1, inf)));

EventPattern((dur: 0.1, degree: [1, 2].pseq)).play;

