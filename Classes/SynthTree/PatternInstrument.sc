/*
IZ Fri, Apr  4 2014, 12:46 EEST

*/

PatternInstrument {
	var <pattern; // A PatternPlayer
	var <instrument; // this will be removed soon
	var <name;
	var <numChannels;
	var <>inputSpecs;   // for controls. May be replaced by method
	// that gets the input specs from the pattern.
	var synthEventAction; // NEW: can act as filter, copy/changing the event
	var bus, busIndex, group;

	*new { | pattern, instrument = \default, name = \pattern, numChannels,
		synthEventAction |
		^this.newCopyArgs(pattern, instrument, name, numChannels, synthEventAction)
		.init;
	}

	init { 
		this.instrument = instrument;
		numChannels ?? { numChannels = ~numChans; };
		synthEventAction ?? { synthEventAction = this.defaultSynthEventAction; }
	}

	defaultSynthEventAction {
		/* Note: This could return an object other than a Function
			For example, one that encapsulates the standard action below, 
			but adds further filters to the event, copying + changing it,
			or choosing to perform other actions also. 
			Thus subclassing or also composition of the action could be possible.
		*/
		^{ | synthEvent |
			synthEvent
			.out_(busIndex)
			.target_(group)
			.addAction_(\addToHead)
			.play;
		}
	}

	set { | param, argPattern | pattern.set(param, argPattern); }

	instrument_ { | argInstrument = \default | pattern.set(\instrument, argInstrument) }
	legato_ { | argLegato | pattern.set(\legato, argLegato) }
	durations_ { | argDurations | pattern.durations = argDurations }
	start { pattern.start }
	stop { pattern.stop }
	isPlaying { ^pattern.isPlaying }
	asSynthTemplate { | argName | name = argName; }
	templateArgs { ^[ControlName(\amp, nil, \control, 1)] }

	asSynth { | synthTree, fadeTime |
		var patternSynth;
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
		this.setSynthEventAction(synthEventAction);
		patternSynth.onEnd(this, { this.objectClosed });
		patternSynth.init(synthTree, bus);
		pattern.start;
		^patternSynth;
	}

	setSynthEventAction { | argSynthEventAction |
		synthEventAction = argSynthEventAction;
		this.addNotifier(this.pattern, \value, synthEventAction);
	}
}
