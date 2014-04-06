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

IZ Tue, Apr  1 2014, 02:58 EEST

================ PatternInstrument: ================

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
	var <>inputSpecs;

	*new { | pattern, instrument = \default, name = \pattern, numChannels |
		^this.newCopyArgs(pattern, instrument, name, numChannels).init;
	}
	init { 
		instrument = instrument.asStream;
		numChannels ?? { numChannels = ~numChans; };
	}

	start { pattern.start }
	stop { pattern.stop }
	isPlaying { ^pattern.isPlaying }

	=> { | chuckee, numChans | ^chuckee.receivePatternInstrument (this, numChans) }

	/* TODO: Interface for chucking to SynthTree: */
	asSynthTemplate { | argName |
		name = argName;
	}

	templateArgs {
		^[ControlName(\amp, nil, \control, 1)]
	}

	asSynth { | synthTree, fadeTime |
		var bus, busIndex, patternSynth, group;
		bus = Bus.audio(synthTree.server, numChannels);
		busIndex = bus.index;
		group = Group(synthTree.group, \addToHead);
		patternSynth = { 
			Inp.ar(numChannels: numChannels).ladsrOut
		}.asPatternSynth(
			target: group,
			addAction: \addToTail,
			args: [in: busIndex, fadeIn: synthTree.getFadeTime, 
				amp: synthTree.getParamValue(\amp)]
		);
		this.addNotifier(this.pattern, \value, { | synthEvent |
			var eventSynth;
			eventSynth = Synth(instrument.next,
				synthEvent.params ++ [out: busIndex],
				group, \addToHead
			);
			pattern.clock.sched(synthEvent.dur, {
				if (patternSynth.isPlaying) { eventSynth.release };
			});
		});
		this.addNotifier(synthTree, \stopped, { this.objectClosed });
		^patternSynth.init(synthTree, bus);
	}
}

SynthPattern {
	//	var <instrument;
	var <params;
	var <legato;

	*new { | /* instrument, */ params, legato = 1 |
		^this.newCopyArgs (/* instrument, */ params, legato);
	}

	asStream { ^SynthStream(/* instrument, */ params, legato) }

	%> { | durations |
		^PatternPlayer(this, durations)
	}
}

SynthStream {
	var <params, <legato;

	*new { | params, legato = 1 |
		^this.newCopyArgs(ParamStream(params), legato.asStream);
	}

	next { | dur |
		^SynthEvent (params.next, legato.next * dur)
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

+ Function {
	asPatternSynth { | target, outbus = 0, fadeTime = 0.02, addAction=\addToHead, args |
		var def, synth, server, bytes, synthMsg;
		target = target.asTarget;
		server = target.server;
		if(server.serverRunning.not) {
			("server '" ++ server.name ++ "' not running.").warn; ^nil
		};
		def = this.asSynthDef(
			fadeTime:fadeTime,
			name: SystemSynthDefs.generateTempName
		);
		synth = PatternSynth.basicNew(def.name, server);
		// if notifications are enabled on the server,
		// use the n_end signal to remove the temp synthdef
		if(server.notified) {
			OSCpathResponder(server.addr, ['/n_end', synth.nodeID], { |time, resp, msg|
				server.sendMsg(\d_free, def.name);
				resp.remove;
			}).add;
		};
		synthMsg = synth.newMsg(target, [\i_out, outbus, \out, outbus] ++ args, addAction);
		def.doSend(server, synthMsg);
		^synth
	}
}

+ Ref {
	receivePatternInstrument { | patternInstrument, numChannels |
		^this.value.receivePatternInstrument(patternInstrument, numChannels);
	}
}

+ Symbol {
	receivePatternInstrument { | patternInstrument, numChannels |
		^this.asSynthTree.receivePatternInstrument(patternInstrument, numChannels);
	}
}

+ SynthTree {
	receivePatternInstrument { | patternInstrument, numChannels |
		^this.chuck(patternInstrument, numChannels);
		// [this.Method.name, "not implemented"].postln;
		//	this.value.asSynthTree receivePatternInstrument: pi;
	}
}