/*
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
		patternSynth.onEnd(this, { this.objectClosed });
		patternSynth.init(synthTree, bus);
		pattern.start;
		^patternSynth;
	}

	=> { | chuckee, numChans | ^chuckee.receivePatternInstrument (this, numChans) }

	chuckParam { | param, pattern |
		pattern.chuckParam(param, pattern)
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

	/*
	%> { | durations |
		^PatternPlayer(this, durations)
	}
	*/
	set { | param, value |
		var index;
		index = params indexOf: param;
		index !? params[index * 2 + 1] = value.asStream;
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

	set { | param, pattern |
		params.set(param, pattern);
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