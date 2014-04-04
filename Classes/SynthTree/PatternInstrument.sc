/*
Hold the instrument for playing a PatternPlayer. 

Is placed as template in SynthTree.

Instrument can be a stream.

Why separate the instrument from the pattern?  In order to play a pattern with different instruments at the same time.  Like in ... *MUSIC*.

IZ Fri, Apr  4 2014, 12:46 EEST

*/

PatternInstrument {
	var <pattern; // A PatternPlayer
	var <instrument;
	var <name;
	var <numChannels;

	*new { | pattern, instrument = \default, name = \pattern, numChannels |
		^this.newCopyArgs(pattern, instrument, name, numChannels).init;
	}
	init { 
		instrument = instrument.asStream;
		numChannels ?? { numChannels = ~numChans; };
	}

	asSynth { | synthTree, fadeTime |
		^PatternSynth(synthTree, numChannels)
	}
}
