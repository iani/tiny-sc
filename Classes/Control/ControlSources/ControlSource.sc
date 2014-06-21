/*

================ ControlSource ================

Source for controlling parameters of a SynthTree. Does one of these 2:
- Sends set messages to the parameter
- Maps the parameter to a control bus that receives input from a Synth.

Fri, Jun 13 2014, 10:11 EEST
*/

ControlSource { // Abstract class
	var parameter; // parameter being controlled
	var template; // template from which the source is created
	var source;   // source of the control

	*new { | parameter template |
		// create new instance
		^this.newCopyArgs(parameter, template);
	}

	start {
		// start the control's process
	}

	stop {
		// stop the control's process
	}

	free {
		// stop and free all resources
		this.objectClosed;
		// subclasses add more stuff as needed.
	}
}

ControlSynth : ControlSource { // kr Synth sending to bus mapped to parameter
	var <bus;

	*new { | parameter, template |
		^super.new(parameter, template).initControlSynth;
	}

	initControlSynth {
		bus = Bus.control(parameter.server, 1);
		this.addNotifier(parameter.synthTree, \started, { parameter bmap: bus });
		parameter.changed(\controlSynth, this);
		this.addNotifier(parameter, \controlSynth, { this.free; });
		this.start;
	}

	start { 
		if (this.isPlaying.not) {
			source = template.kr(bus.index);
			source.onEnd(this, { source = nil });
			source.onStart(this, { parameter bmap: bus });
		}
	}

	stop { if (this.isPlaying) { source.free } }
	isPlaying { ^source.notNil }

	free {
		bus !? {
			this.stop;
			bus.free;
			bus = nil;
			super.free;
		};
	}
}


ControlMIDI : ControlSource { // set parameter based on MIDI input
	*new { | parameter specs |
		// Store specs as template.
		// Create MIDIFunc from specs as source; 
	}
}

ControlOSC : ControlSource { // set parameter based on OSC input

	*new { | parameter specs |
		// Store specs as template.
		// Create OSCFunc from specs as source; 
	}	
}

+ Function {
	makeControlSynth { | busIndex = 0 server |
		^this.kr(busIndex, server)
	}

	kr { | outBus = 0 outName = \out server |
		^{ Out.kr(outName.kr(0), this.value) }.play(server, args: [outName, outBus]);
	}
	/* Tested - now replaed by addKrFunc
	+> { | st, param |
		^ControlSynth(st.asSynthTree.getParam(param), this);
	}
	*/
	// Used to be ++> while under testing. 
	+> { | st param |
		// THIS WILL REPLACE +> ABOVE, AFTER BEING TESTED
		^st.asSynthTree.getParam(param).addKrFunc(this);
	}
}

/*
** TODO Control Sources (+MultiControl: Single control source)
:PROPERTIES:
:DATE:     <2014-06-13 Fri 09:24>
:END:
source
Simplify the way in which diverse controls are added to a MultiControl instance.
Possibly make MultiControl a base Class - not an IdentityDictionary.

*** two types of control sources:

**** =ControlSource= unnamed, not-shared control sources

- added to a source's parameter by object:

#+BEGIN_EXAMPLE
{ function } +>.paramname \sourcetree;

number +>.paramname \sourcetree;

buffer +>.paramname \sourcetree;

`\bufname +>.paramname \sourcetree;

MIDIfunc +>.paramname \sourcetree;

OSCfunc +>.paramname \sourcetree;

Event +>.paramname \sourceree;

#+END_EXAMPLE

- when a new control source is added, the previous one is freed.

- starting and stopping or freeing of the control source is independent of the starting and stopping of the controlled parameter's sourcetree.   However, there are explicit messages / operators for starting, stopping or freeing or removing of a control of a parameter.

- implementations for starting, stopping, freeing and for initializing (reconnecting) when controlled source restarts are coded by subclassing.

**** =SharedControlSource= named, registered, shared control sources

- added to a sourcetree's parameter by name:

: \controlsource +>.paramname \sourcetree;

- created and registered in global register, using Registry

- Connected to listeners through =Notification=, broadcast their changes through the =changed= message mechanism.


*** instance var control:

may contain one of:
- MIDIfunc
- EventStream
- OSCfunc
- Bus
- A protean kind of broadcasting control source of yet undefined class (not yet implemented).

when setting control:

1. Disconnect previous control.
2.


*/

