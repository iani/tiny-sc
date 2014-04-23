/*

Enables the playing of the same Bdef in multiple SynthTrees, 
optionally applying modifications on the events created by the Bdef, before playing.

A separate insrance resides as template inside a SynthTree.

*/

BdefInstrument {
	var <bdef; // A BdefEventPlayer
	Var: <numChannels;
	var <>inputSpecs;   // for controls. May be replaced by method
	// that gets the input specs from the bdef.
	var <>synthEventActionMaker; // NEW: can act as filter, copy/changing the event
	var <>eventFilter; // array of params with bdefs modifying received event
	var bus, busIndex, group;

	*new { | bdef, numChannels, synthEventAction |
		^this.newCopyArgs(bdef, numChannels, synthEventAction)
		.init;
	}

	name { ^bdef.name ? "a bdef" }

	init { numChannels ?? { numChannels = ~numChans; } }
	set { | params | bdef.set(params); }
	instrument_ { | argInstrument = \default | bdef.set([\instrument, argInstrument]) }
	legato_ { | argLegato | bdef.set([\legato, argLegato]) }
	durations_ { | argDurations |
		// TODO: fix this.  One source only!
		// BdefPlayer should get its durations from the bdef array
		bdef.durations = argDurations;
		bdef.set([\dur, argDurations]);
	}
	start { bdef.start }
	stop { bdef.stop }
	isPlaying { ^bdef.isPlaying }
	asSynthTemplate { | argName | name = argName; }
	templateArgs { ^[ControlName(\amp, nil, \control, 1)] }

	asSynth { | synthTree fadeTime |
		var bdefSynth;
		bus = Bus.audio(synthTree.server, numChannels);
		busIndex = bus.index;
		group = Group(synthTree.group, \addToHead);
		bdefSynth = { 
			Inp.ar(numChannels: numChannels).ladsrOut
		}.asBdefSynth(
			target: group,
			addAction: \addToTail,
			args: [in: busIndex, fadeIn: synthTree.getFadeTime, 
				amp: synthTree.getParamValue(\amp), out: synthTree.getOutputBusIndex]
		);
		this.setSynthEventAction;
		this.addNotifier(bdef, \taskStopped, { synthTree.fadeOut; });
		bdefSynth.init(synthTree, bus);
		bdef.start;
		^bdefSynth;
	}

	/*
		clear: clear BdefEventPlayer 
		remove: Remove all connections to other objects. Ready for GC.
		reset: remove + return new empty instance to play 
	*/
	clear { bdef.clear; }  // TODO: TEST THESE
	reset {  // TODO: TEST THESE
		this.remove;
		^this.class.new;
	}
	remove { this.objectClosed; } // TODO: TEST THESE
	
	setSynthEventAction { | actionMaker |
		/* TODO: use synthEventActionMaker to create the action 
			Here we can for example make the bdef play Pmono-style.
		*/
		var action;
		if (synthEventActionMaker.isNil) {
			action = this.defaultSynthEventAction;
		}{
			action = synthEventActionMaker.(busIndex, group);
		};
		this.addNotifier(bdef, \value, action);
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