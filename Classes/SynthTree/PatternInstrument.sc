/*
Hold the instrument for playing a PatternPlayer. 

Instrument can be a stream.

Why separate the instrument from the pattern?  In order to play the same pattern with different instruments at the same time, like in *MUSIC*.

IZ Fri, Apr  4 2014, 12:46 EEST

*/

PatternInstrument {
	var <pattern, <instrument;

	*new { | pattern, instrument = \default |
		^this.newCopyArgs(pattern, instrument).init;
	}
	init { 
		instrument = instrument.asStream;
	}
}
