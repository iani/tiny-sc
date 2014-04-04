/*
Play synths in a SynthTree, timed by a duration stream, with parameters
generated from an array of patterns.  


(Note to self: Forget about nesting here.  For that you need EventList.)

The valueStream may produce as value an array of 3 elements:

1. Name of synthdef to play, or nil for silence.
2. Args array for the synth, eg: [\freq, 440, \amp, 0.1 ... etc]. The SynthTree adds the target group, addAction, and output/input parameters to create the synth.
3. Duration after which the SynthTree should release the synth.  The SynthTree schedules a function roughly like this:

: SystemClock.sched(dur, { synth.release })

or:

: aTempoClock.sched(dur, { synth.release })

Dur may be different from the delta time for the next event of the pattern, depending on legato.

Following this through the three stages patern -> stream -> next value:

Pattern should contain:
- instrumentpattern
- parampattern
- legatopattern

These three are converted to streams and put into a SynthStream, which creates 
SynthEvents to play.  So we have following classes:

1. SynthPattern - contains the pattern producing the stream
2. SynthStream - contains the stream producing the event
3. SynthEvent - contains the parameters for creating the Synth

We don't need a SynthPatternPlayer

IZ Tue, Apr  1 2014, 02:58 EEST

*/

SynthPattern {
	//	var <instrument;
	var <params;
	var <legato;

	*new { | /* instrument, */ params, legato = 1 |
		^this.newCopyArgs (/* instrument, */ params, legato);
	}

	asStream { ^SynthStream(/* instrument, */ params, legato) }

	%> { | durations |
		^PatternPlayer (this, durations)
	}
}

SynthStream {
	var /* <instrument, */ <params, <legato;

	*new { | /* instrument, */ params, legato = 1 |
		^this.newCopyArgs(/* instrument.asStream, */ ParamStream(params), legato.asStream);
	}

	next { | dur |
		^SynthEvent (/* instrument.next, */ params.next, legato.next * dur)
	}
}

ParamStream {
	var <keys, <values;

	*new { | params |
		^super.new.init(params)
	}

	init { | params |
		#keys, values = params.clump(2).flop;
		values = values collect: _.asStream;
	}

	next {
		^[keys, values collect: _.next].flop.flat;
	}

	set { | param, value |
		var index;
		index = keys indexOf: param;
		index !? values[index] = value.asStream;
	}
}

SynthEvent {
	var /* <instrument, */ <params, <dur;
	var <synth;

	*new { | /* instrument, */ params, dur |
		^this.newCopyArgs (/* instrument, */ params, dur);
	}
}