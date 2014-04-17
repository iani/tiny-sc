/*
IZ Fri, Apr  4 2014, 12:46 EEST

*/

PatternInstrument {
	var <pattern; // A PatternPlayer
	var <name;
	var <numChannels;
	var <>inputSpecs;   // for controls. May be replaced by method
	// that gets the input specs from the pattern.
	var <>synthEventAction; // NEW: can act as filter, copy/changing the event
	var <>eventFilter; // array of params with patterns modifying received event
	var bus, busIndex, group;

	*new { | pattern, instrument = \default, name = \pattern, numChannels,
		synthEventAction |
		^this.newCopyArgs(pattern, name, numChannels, synthEventAction)
		.init;
	}

	init { 
		numChannels ?? { numChannels = ~numChans; };
	}

	set { | params | pattern.set(params); }

	instrument_ { | argInstrument = \default | pattern.set([\instrument, argInstrument]) }
	legato_ { | argLegato | pattern.set([\legato, argLegato]) }
	durations_ { | argDurations |
		// TODO: fix this.  One source only!
		// PatternPlayer should get its durations from the pattern array
		pattern.durations = argDurations;
		pattern.set([\dur, argDurations]);
	}
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
		this.addNotifier(this.pattern, \value, { | synthEvent |
			synthEvent
			.out_(busIndex)
			.target_(group)
			.addAction_(\addToHead)
			.play;
		});		
		//		this.setSynthEventAction(SynthEventAction(busIndex, group, addAction, ));
		//		this.setSynthEventAction;
		patternSynth.onEnd(this, { this.objectClosed });
		patternSynth.init(synthTree, bus);
		pattern.start;
		^patternSynth;
	}

	setSynthEventAction {
		// primitive solution:
		var theAction;
		if (synthEventAction.notNil) {
			theAction = synthEventAction
		}{
			if (eventFilter.notNil) {
				theAction = SynthEventAction(busIndex, group, \addToHead, eventFilter);
			}{
				theAction = { this.defaultSynthEventAction }
			}
		};
		this.addNotifier(this.pattern, \value, theAction);
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
}

SynthEventAction {
	var <>out, <>target, <>addAction;
	var <>eventFilter, <playEvent;

	*new { | out, target, addAction, filters |
		^this.newCopyArgs(out, target, addAction, filters);
	}

	value { | synthEvent |
		playEvent = (out: out, target: target, addAction: addAction).parent_(synthEvent);
		playEvent use: {
			eventFilter keysValuesDo: { | param, stream |
				playEvent[param] = stream.next(playEvent);
			}
		};
		playEvent.play;
	}
}
