/*
A Bdef that is triggered via OSC from trigger messages issued from a synth, 
rather than by a routine. 

Idea by Yorgos Diapoulis.

IZ Fri, Apr 25 2014, 16:04 EEST

Here tests.

*/


{ WhiteNoise.ar } => \test;

Server.default.boot;

(degree: 10) => \asdf;

//:

Idef

//:
a = Trigdef.fromEvent((degree: 10));
//:
BdefInstrument(a) => \trigdef;
//:
\trigdef.asSynthTree.template.bdef.test;

