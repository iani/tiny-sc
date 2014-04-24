

a = Edef(\x, (degree: 20, dur: 0.5, amp: 0.5));
b = Edef(\x).broadcast;
c = \synthtree.asSynthTree;
d = BdefInstrument(b);
c.template = d;
c.start;

//: //////////////////////////////////////////////////////////////



a = Edef(\x, (degree: 20, dur: 0.5, amp: 2));
b = Edef(\x).broadcast;
c = \synthtree.asSynthTree;
d = BdefInstrument(b);

c.chuck(d);


//:

Edef(\x, (degree: 20, dur: 0.5, amp: 2)) => \test;

//:
3 =!> \fadeTime;

(degree: [20, 22].pseq, dur: 0.5) => \test2;

a = \test2.asSynthTree.template;

a.bdef addDependant: { | ... args | args.postln; };

a.isPlaying;
a.bdef.isPlaying;
a.bdef.stop;

\test2.fadeOut;


(degree: [10, 11].pseq, dur: 0.5) => \test2;

\test2.asSynthTree.template.stop;

a === \test2.asSynthTree.template;
a
a.stop;

\test2.asSynthTree.template;
a;

\test2.asSynthTree.template.mods = (degree: Pwhite(1, 20, inf));

(dur: 5).play;

(play: { "Hello World".postln; }).play;

Bdef

{ WhiteNoise.ar } => \test2;

{ SinOsc.ar(500) } => \test2;

