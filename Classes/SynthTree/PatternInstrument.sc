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
		this.instrument = instrument;
		numChannels ?? { numChannels = ~numChans; };
	}

	set { | param, argPattern |
		pattern.set(param, argPattern);
	}

	instrument_ { | argInstrument = \default | instrument = argInstrument.asStream }
	legato_ { | argLegato | pattern.legato = argLegato }
	durations_ { | argDurations |
		pattern.durations = argDurations
	}

	start { pattern.start }
	stop { pattern.stop }
	isPlaying { ^pattern.isPlaying }
	
	asSynthTemplate { | argName |
		name = argName;
	}

	templateArgs {
		^[ControlName(\amp, nil, \control, 1)]
	}

	// TODO: Fix initTree to work correctly for sending to fx inputs
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
				amp: synthTree.getParamValue(\amp), out: synthTree.getOutputBusIndex]
		);
		this.addNotifier(this.pattern, \value, { | synthEvent |
			var eventSynth;
			eventSynth = Synth(instrument.next,
				synthEvent.params ++ [out: busIndex],
				group, \addToHead
			);
			pattern.clock.sched(synthEvent.dur max: 0.02, {
				if (patternSynth.isPlaying) { eventSynth.release };
			});
		});
		patternSynth.onEnd(this, { this.objectClosed });
		patternSynth.init(synthTree, bus);
		pattern.start;
		^patternSynth;
	}
}

SynthPattern {
	//	var <instrument;
	var <params;
	var <>legato;

	*new { | /* instrument, */ params, legato = 1 |
		^this.newCopyArgs (/* instrument, */ params, legato);
	}

	asStream { ^SynthStream(/* instrument, */ params, legato) }

	set { | param, value |
		var index;
		index = params indexOf: param;
		if (index.isNil) {
			params = params ++ [param, value];
		}{
			params[index + 1] = value;
		}
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

	legato_ { | argLegato | legato = argLegato.asStream; }
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
		if (index.isNil) {
			keys = keys add: param;
			values = values add: value.asStream;
		}{
			values[index] = value.asStream;
		}
	}
}

SynthEvent {
	var /* <instrument, */ <params, <dur;
	var <synth;

	*new { | /* instrument, */ params, dur |
		^this.newCopyArgs (/* instrument, */ params, dur);
	}
}