/*
Designing in detail how Edef clones into Idef, Bdef, and into SynthTree.

Sat, Jun  7 2014, 22:04 EEST

*/


//: \symbol =< (event) creates and starts an Edef named by the symbol
\x =< (freq: Pwhite(300, 900, inf), dur: 0.1);

//: subsequent symbol =< event statements modify the Edef
\x =< (dur: 0.3);

//: => create 
\x => \y;

//: The 
\x => \z;

//: Modifications to the original are propagated to all cloned streams
\x =< (freq: Pwhite(200, 300, inf), legato: 0.1);

//: Stop all streams cloned from \x. 
\x.stopAll;

//: 
\x => \st1.st; // st = shortcut for asSynthTree



\x => \st2;

\x =< (legato: 0.1, dur: [0.1, 0.4].prand);

//:

Edef(\x).inspect;