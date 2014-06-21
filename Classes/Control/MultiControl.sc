/*
Enable control of a SynthTask's synth parameters from multiple sources: 
Views, patterns, busses, MIDIFuncs, OSCFuncs, etc.

Each source is stored under a name in a dictionary (controls).  
It can be individually enabled/disabled, changed or removed. 

IZ Tue, Mar 11 2014, 18:10 EET
*/

SynthTreeArgs : /* IdentityDictionary */ Event {
	var <synthTree;
	var <event;

	*new { | synthTree | ^super.new.init(synthTree); }

	init { | argSynthTree |
		var myServer;
		synthTree = argSynthTree;
		myServer = synthTree.server;
		parent = parentEvents[myServer] ?? { SynthTree.makeParentEvent(myServer) };
	}

	storeArgValue { | key, value |
		this.getParam(key).storeValue(value);
	}

	getParamValue { | paramName |
		var param;
		param = this [paramName];
		if (param.isNil) { ^nil } { ^param.nextValue }
	}

	getParam { | key, spec, initialValue, stream |
		var param;
		param = this[key];
		param ?? {
			param = MultiControl(synthTree, key, spec, initialValue, stream);
			this[key] = param;
		};
		^param;
	}

	makeParam { | key, spec |
		[this, thisMethod.name, "not yet implemented"].postln;
	}
}

MultiControl : IdentityDictionary {
	var <synthTree; // SynthTree to which I belong
	var <name;     // name of synth parameter to control
	var <>spec;    // spec to map incoming values from controls
	var <>stream;  // if not nil, provides next value instead of nextValue var.
	var <>nextValue; // next value to use for that parameter.
	// control objects are stored in self as subclass of IdentityDictionary
	// var <>controls; // dictionary holding any control objects 
	//	(MIDIFuncs, OSCFuncs, ViewFuncs, Busses etc.)
	/* Note: Other sources, such as: 
		Patterns, StreamPatterns and PatternTasks should be stored globally
		each in its own dict, and added to any number of SynthTrees.
		One SynthTree might want to compose the stream source
		used by another SynthTree with a second stream source!
	*/
	var <unmappedValue; // cache of unmappedValue for views
	/* New.  Will replace dictionary entry. MultiControl to become base class.: */
	// Wed, Jun 18 2014, 17:44 EEST
	var <krMap; // only one kr Source at any moment
	var <scalarSources; // any number of scalar (number) sources

	*new { | synthTree, name, spec, initialValue, stream |
		^super.new.initMultiControl(synthTree, name, spec, initialValue, stream);
	}

	initMultiControl { | argSynthTree, argName, argSpec, initialValue, argStream |
		synthTree = argSynthTree;
		name = argName;
		spec = (argSpec ? name).asSpec ? NullSpec;
		stream = argStream.asStream;
		nextValue = initialValue ?? { stream.next ?? { spec.default } };
		unmappedValue = spec unmap: nextValue;
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
		this.set(spec map: value, value);
	}

	receiveNumberChuck { | number |
		this.set (number);
	}

	set { | value, argUnmappedValue |
		synthTree.setSynthParameter(name, value);
		this.storeValue(value, argUnmappedValue);
	}

	storeValue { | value, argUnmappedValue |
		nextValue = value;
		unmappedValue = argUnmappedValue ?? {
			spec unmap: value;
		};
		this.changed(\value, value, unmappedValue);
	}

	receivePatternChuck { | pattern |
		// TODO: fix this to work with [100, 200].pseq => ~freq;
		// should be like this: 
		// this.playPattern(pattern.asPatternTask);
		this.playPattern (pattern); // not like this!
	}

	playPattern { | pattern, controlName = \pattern, 
		startTogether = true, stopTogether = true |
		var pFunc;
		pFunc = this[controlName];
		pFunc !? { pFunc.remove };
		pFunc = PatternFunc(pattern, this, { | value |
			this.set(value);
		});
		this[controlName] = pFunc;
		if (synthTree.isPlaying and: { pattern.isPlaying.not }) { pattern.start };
		if (startTogether) {
			pFunc.addNotifier(synthTree, \started, { pFunc.pattern.start });
		};
		if (stopTogether) {
			pFunc.addNotifier(synthTree, \stopped, { pFunc.pattern.stop });
		}
	}

	getPattern { | patternName = \pattern | ^this[patternName].pattern }

	addKrFunc { | func, sourceName = \source |
		// play func as kr source into KrMap.  Create map if needed
		var newSource;
		krMap ?? {
			krMap = Registry(synthTree, this, { KrMap.newKr(synthTree.server) });
		};
		newSource = krMap.addSource(func, name, startNow: true);
		this.addNotifierOneShot(newSource, \started, { this bmap: krMap });
	}

	addKrMap { | argKrMap |
		// map to krMap.  
		krMap = argKrMap;
		this bmap: krMap;
	}

	bmap { | bus | // map to bus
		krMap = bus;
		if (synthTree.isPlaying) { synthTree.synth.map(name, bus.index); };
		this.addNotifier(synthTree, \started, { synthTree.synth.map(name, bus.index) });
	}

	bunMap { // unmap from bus and remove bus
		krMap !? { krMap get: { | busValue | this.set(busValue) } };
		krMap = nil;
		this.removeNotifier(synthTree, \started);
	}

	// ================ following should be reviewed ================
	add { | controlName, control |

	}

	get { | controlName |

	}

	remove { | controlName |
		// remove a control.  Disable it first
		var ctl;
		ctl = this[controlName];
		if (ctl.notNil) {
			ctl.disable;
			this[controlName] = nil;
		};
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

	// ================ above should be reviewed ================
	// reset a control's value to specs default
	reset { this.set(spec.default) }

	// adding specific kinds of objects
	addMIDI { | name, spec, func |
		
	}

	addOSC { | name, path, func |
		
	}

	addView { | argName, view, func, onClose, enabled = true |
		argName = argName ? name;
		// view = view ?? { Knobs.knob(argName, synthTree.name) };
		this[argName] = ViewFunc(
			this, // only one ViewFunc is added per argName
			view ?? {
				this.connectParamView(
					Knobs.knob(argName, synthTree.name)
				)
			},
			func ?? {{ | value | this.set(spec.map(value)) }},
			onClose ?? {{ this.remove(argName) }},
			enabled
		).value_(spec unmap: nextValue);
	}

	connectParamView { | view |
		view.addNotifier(synthTree, \started, { | ... args |
			view.setPlaying;
		});
		view.addNotifier(synthTree, \stopped, { | ... args |
			//			if (synthTree.isPlaying) {} { view.setStopped; };
			view.setStopped;
		});
		view.addNotifier(synthTree, \fadeOut, { | ... args |
			//			if (synthTree.isPlaying) {} { view.setFadeOut; };
			view.setFadeOut;
		});
		if (synthTree.isPlaying) { 
			view.setPlaying;
		};
		view.keyDownAction = { | view, char, modifiers, unicode, keycode, key |
			switch (char,
				$g, { synthTree.start },
				$G, { synthTree.trig },
				$s, { synthTree.fadeOut },
				$S, { synthTree.free },
				$f, { synthTree.free }, // Better synonym
				$k, { synthTree.knobs },
				$b, { synthTree.bufferList },
				$,, { thisProcess.stop },
				$., { SynthTree.stopAll },
				$i, { synthTree.inspect },
				Char.space, { synthTree.toggle },
				{ view.defaultKeyDownAction(
					char, modifiers, unicode, keycode, key) 
				}
			)
		};
		view.addNotifier(this, \value, { | value, unmappedValue |
			{ view.value = unmappedValue; }.defer;
		});
		view.value = unmappedValue ?? { spec unmap: nextValue };
		^view;
	}

	// ================================================================
	// Controls.  Redoing: Fri, Jun 13 2014, 08:51 EEST

	server { ^synthTree.server }
	
	addKr { | argSource |
		// add a kr control bus.  Removes previous bus!
		argSource = argSource.asKrSource(this);
		if (krMap === argSource) { ^this };
		this.removeKr;
		krMap = argSource;
		krMap addTo: this;
	}

	removeKr {
		krMap removeFrom: this;
		krMap = nil;
	}
}