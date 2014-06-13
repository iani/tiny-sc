/*
A PatternInstrument lives as a template inside a SynthTree and 
It holdes a PatternEventPlayer and responds to its changed messages 
by creating synths that play into a separate bus.  

A PatternInstrument can only play in one single SynthTree. 
To share data control amongst synthtrees, different instances of PatternInstrument
can be made to respond to changed messages from the same PatternEventPlayer. 

The different PatternInstrument instances can vary the way in which they
interpret the messages from the PatternEventPlayer by setting different synthEventActions.

IZ Fri, Apr  4 2014, 12:46 EEST
*/

PatternInstrument {
	var <pattern; // A PatternEventPlayer
	var <name;
	var <numChannels;
	var <>inputSpecs;   // for controls. May be replaced by method
	// that gets the input specs from the pattern.
	var <>synthEventActionMaker; // NEW: can act as filter, copy/changing the event
	var <>eventFilter; // array of params with patterns modifying received event
	var bus, busIndex, group;

	*new { | pattern, name = \pattern, numChannels,
		synthEventAction |
		^this.newCopyArgs(pattern, name, numChannels, synthEventAction)
		.init;
	}

	init { numChannels ?? { numChannels = ~numChans; } }
	set { | params | pattern.set(params); }
	instrument_ { | argInstrument = \default | pattern.set([\instrument, argInstrument]) }
	legato_ { | argLegato | pattern.set([\legato, argLegato]) }
	durations_ { | argDurations |
		// TODO: fix this.  One source only!
		// PatternTask should get its durations from the pattern array
		pattern.durations = argDurations;
		pattern.set([\dur, argDurations]);
	}
	start { pattern.start }
	stop { pattern.stop }
	isPlaying { ^pattern.isPlaying }
	asSynthTemplate { | argName | name = argName; }
	templateArgs { ^[ControlName(\amp, nil, \control, 1)] }

	asSynth { | synthTree fadeTime |
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
		this.setSynthEventAction;
		this.addNotifier(pattern, \taskStopped, { synthTree.fadeOut; });
		patternSynth.init(synthTree, bus);
		pattern.start;
		^patternSynth;
	}

	/*
		clear: clear PatternEventPlayer 
		remove: Remove all connections to other objects. Ready for GC.
		reset: remove + return new empty instance to play 
	*/
	clear { pattern.clear; }  // TODO: TEST THESE
	reset {  // TODO: TEST THESE
		this.remove;
		^this.class.new;
	}
	remove { this.objectClosed; } // TODO: TEST THESE
	
	setSynthEventAction { | actionMaker |
		/* TODO: use synthEventActionMaker to create the action 
			Here we can for example make the pattern play Pmono-style.
		*/
		var action;
		if (synthEventActionMaker.isNil) {
			action = this.defaultSynthEventAction;
		}{
			action = synthEventActionMaker.(busIndex, group);
		};
		this.addNotifier(pattern, \value, action);
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
