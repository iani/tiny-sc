/*
A stream with side effects.
Starting point for implementing PatternEventPlayer using embedInStream.

IZ Mon, Apr 21 2014, 09:58 EEST

SideEffectPattern().play;

*/

SideEffectPattern : Pattern {

	asStream { ^SideEffectStream() }

}

SideEffectStream : Stream {

	next { ^() }
	
	// NO effect? Why?
	embedInStream { arg inval;
		var outval;
		while {
			true;
		}{
			100.rand.postln;
			outval.yield;
		};
		^0.1
	}
	
}