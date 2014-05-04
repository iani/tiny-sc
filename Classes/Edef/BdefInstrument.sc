/*

Enables the playing of the same Bdef in multiple SynthTrees, 
optionally applying modifications on the events created by the Bdef, before playing.

A different instance resides as template inside each SynthTree.

*/

BdefInstrument {
	var <bdef; // A BdefEventPlayer
	var <numChannels;
	var <>inputSpecs;   // for controls. May be replaced by method
	// that gets the input specs from the bdef.
	var <>eventFilter; // array of params with bdefs modifying received event
	var bus, busIndex, group;
	var <>mods; // optional local modifiations to event
	var <>stopBdefOnSynthEnd = true;

	// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	// maybe this is too much complexity/power/flexibility. Maybe just use mods:
	var <>synthEventActionMaker; // ??? can act as filter, copy/changing the event

	*new { | bdef, numChannels, synthEventAction |
		^this.newCopyArgs(bdef, numChannels, synthEventAction).init;
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
	start { bdef.play; }
	stop { bdef.stop; }
	isPlaying { ^bdef.isPlaying }
	asSynthTemplate { /* ^this */ }
	templateArgs { ^[ControlName(\amp, nil, \control, 1.0)] }

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
		bdefSynth.onEnd(this, {
			this.removeNotifier(bdef, \event);
			if (stopBdefOnSynthEnd) { bdef.stop; };
		});
		this.setSynthEventAction;
		this.addNotifier(bdef, \taskStopped, { 
			if (synthTree.template === this) { synthTree.fadeOut };
		});
		bdefSynth.init(synthTree, bus);
		bdef.play;
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
		this.addNotifier(bdef, \event, action);
	}

	defaultSynthEventAction {
		/* Note: This could return an object other than a Function
			For example, one that encapsulates the standard action below, 
			but adds further filters to the event, copying + changing it,
			or choosing to perform other actions also. 
			Thus subclassing or also composition of the action could be possible.
		*/
		^{ | synthEvent |
			mods.applyMods(synthEvent.copy.make({
				~out = busIndex;
				~target = group;
				~addAction = \addToHead
			})).play;
		}
	}

	addPlayerMods { | event | bdef.addEvent(event); }
	replacePlayerMods { | event | bdef.replaceEvent(event); }
	addMods { | event | mods = mods addStreamMods: event; }
	replaceMods { | event | mods = nil addStreamMods: event }	
}

+ Nil {
	addPatternMods { | event | ^() addPatternMods: event; }
	addStreamMods { | event | ^() addStreamMods: event; }
	applyMods { | event | ^event }
}

+ Event {
	
	addPatternMods { | event |
		event keysValuesDo: { | key value | event[key] = value }
	}

	addStreamMods { | event |
		event keysValuesDo: { | key value | this[key] = value.asStream }			
	}

	applyMods { | event |
		event use: { // accepts functions as well as streams
			this keysValuesDo: { | key value | event[key] = value.value };
		};
		^event;
	}
}

+ Function {

	applyMods { | event | ^this.(event) }

	asBdefSynth { | target, outbus = 0, fadeTime = 0.02, addAction=\addToHead, args |
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