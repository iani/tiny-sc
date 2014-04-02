//: 03 Mar 2014 13:19:01
//: From Pfset help:

(
// Synthdef for the tests:
SynthDef(\sinegrain,
    { arg out = 0, freq = 440, sustain = 0.02;
        var env;
        env = EnvGen.kr(Env.perc(0.001, sustain), 1, doneAction:2);
        Out.ar(out, SinOsc.ar(freq, 0, env * 0.1))
    }).add;
)

//: Example, modified to play faster:
(
a = Pbind(\dur, 0.1, \instrument, \sinegrain, \x, Pfunc { rrand(500, 600) });
a = Pfset({ ~freq = { (~x.postln * 2); }; ~legato = 3; },  a);
a.play;
)
//: Composing with Pfunc works:
(
a = Pbind(\dur, 0.1, \instrument, \sinegrain, \x, Pfunc { rrand(500, 600) });
a = Pfset({ ~freq = { (~x * Pfunc({ 1 + 4.rand }).asStream); }; ~legato = 3; },  a);
a.play;
)
//: Composing with Pseq does not work:
(
a = Pbind(\dur, 0.1, \instrument, \sinegrain, \x, Pfunc { rrand(500, 600) });
a = Pfset({ ~freq = { (~x * Pseq([1, 2, 3], inf).asStream); }; ~legato = 3; },  a);
a.play;
)

//: Composing with Prand works
(
a = Pbind(\dur, 0.1, \instrument, \sinegrain, \x, Pfunc { rrand(500, 600) });
a = Pfset({ ~freq = { (~x * Prand([1, 2, 3], inf).asStream); }; ~legato = 3; },  a);
a.play;
)

//: Composing with Pser does not work
(
a = Pbind(\dur, 0.1, \instrument, \sinegrain, \x, Pfunc { rrand(500, 600) });
a = Pfset({ ~freq = { (~x * Pser([1, 2, 3], inf).asStream); }; ~legato = 3; },  a);
a.play;
)

//: Composing with Pseries does not work
(
a = Pbind(\dur, 0.1, \instrument, \sinegrain, \x, Pfunc { rrand(500, 600) });
a = Pfset({ ~freq = { (~x + Pseries(0, 10, inf).asStream); }; ~legato = 3; }, a);
a.play;
)

//: Without Pfset

a = Pbind(\dur, 0.1, \instrument, \sinegrain, \legato, 3,
    \freq, Pwhite(500, 600, inf) * Pseq([1, 2, 3], inf));
a.play;


//: Composing the pattern without Pbind
a = (Pfunc({ rrand(500, 600) }) + Pseq([1, 2, 3], inf)).asStream;
a.nextN(20);
//:
a = (Pwhite(500, 600, inf) * Prand([1, 2, 3], inf)).asStream;
a.nextN(20);

//:
a = (Pwhite(500, 600, inf) * Pseq([1, 2, 5], inf)).asStream;
a.nextN(20);
