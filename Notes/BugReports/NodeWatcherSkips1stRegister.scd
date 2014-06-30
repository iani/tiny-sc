/*

This bug appears only in certain unreleased alpha versions of 3.7, 
Report date: Sat, Jun 28 2014, 14:44 EEST

Recompile SC, start the default server, and then run the following: 

*/
(
{
	a = { WhiteNoise.ar(0.01) }.play;
	NodeWatcher.register(a);
	0.2.wait;
	a addDependant: { | synth msg | ["THE FIRST TIME IS SKIPPED:", msg].postln; };
	0.2.wait;
	a.free;
	0.2.wait;
	a = { WhiteNoise.ar(0.01) }.play;
	NodeWatcher.register(a);
	0.2.wait;
	a addDependant: { | synth msg | ["THE SECOND TIME IS OK:", msg].postln; };
	0.2.wait;
	a.free;
}.fork;
)