/*
Enable control of a SynthTask's synth parameters from multiple sources: 
Views, patterns, busses, MIDIFuncs, OSCFuncs, etc.

Each source is stored under a name in a dictionary (controls).  
It can be individually enabled/disabled, changed or removed. 

IZ Tue, Mar 11 2014, 18:10 EET
*/

SynthTreeArgs : Event {
	storeArgValue { | key, value |
		var argCtl;
		argCtl = this[key];
		if (argCtl.isNil) {
			this[key] = argCtl = MultiControl(this, key);
		};
		argCtl.nextValue = value;
	}
}

MultiControl : IdentityDictionary {
	var <synthTree; // SynthTree to which I belong
	var <name;     // name of synth parameter to control
	var <>spec;    // spec to map incoming values from controls
	var <>stream;  // if not nil, provides next value instead of nextValue var.
	var <>nextValue; // next value to use for that parameter. Can be a stream
	// control objects are stored in self as subclass of IdentityDictionary
	// var <>controls; // dictionary holding any control objects (StreamPatterns, 
	//	MIDIFuncs, OSCFuncs, Busses, Views etc.)
	
	*new { | synthTree, name, spec, stream, initialValue |
		^this.newCopyArgs(synthTree, name, spec, stream, initialValue).init;
	}

	init {
		spec = spec.asSpec ? NullSpec;
		nextValue = nextValue ?? { spec.default };
	}

	synthArgs {
		/* Return name + next value, for constructing Synth args. */
		^[name, this.next];
	}

	next { 
		/*  Called when starting the synth to get arg parameters.
			If there is a stream, get the value from the stream.
			Otherwise get the stored value.
		*/
		if (stream.notNil) {
			nextValue = stream.next;
			^nextValue;
		}{
			^nextValue;
		}
	}

	mapSet { | value |
		// map the value received from MIDI or view etc. from 0-1 range 
		// to desired range
		this.set(spec map: value);
	}

	set { | value |
		nextValue = value;
		synthTree.setSynthParameter(name, value);
	}

	map { | bus |
	}

	add { | controlName, control |

	}

	get { | controlName |

	}

	remove { | controlName |
		// remove a control.  Disable it first

	}

	enable { | controlName, start = false |
		// enable a control to send me messages
		// If start is true, then also start a control's process

	}

	disable { | controlName, stop = false |
		// disable a control to send me messages
		// If stop is true, then also stop a control's process

	}

	start { | controlName |
		// start a control's process, if appropriate
		// works for StreamPatterns and Synths
	}

	stop { | controlName |
		// stop a control's process, if appropriate
	}

	reset { | controlName, start = false |
		// reset a control's process, if appropriate
		// If start is true, then also start a control's process

	}

	// adding specific kinds of objects
	addMIDI { | name, spec, func |
		
	}

	addOSC { | name, path, func |
		
	}

	addView { | argName, view, func, onClose, enabled = true |
		argName = argName ?? { format("%:%", synthTree.name, name).asSymbol };
			this[argName] = view ?? { Knobs.connect(this, argName) };
		
		/*
		this.controls[argName] = ViewFunc(
			view,
			func ?? {{ | value | this.set(spec.map(value)) }},
			onClose ?? {{ this.remove(argName) }},
			enabled
		)
		*/
	}

	senderClosed { | sender |
		// find sender from values of dict and remove it

	}

	addSynth { | name, template |

	}

	addBus { | name, bus |

	}
}