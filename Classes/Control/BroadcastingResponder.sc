/*
Holds an OSCFunc or MIDIfunc and makes it broadcast with =changed= method. 

Also holds spec.  Registers with Registry, and frees the previous func 
when a new one is to be stored under the same name.

Draft

TODO: Make subclasses for OSCFunc and for the different types of MIDIFuncs
(note-on, control etc.), which control SynthTree parameters in different ways.

Fri, Jun 20 2014, 22:39 EEST
*/

 BroadcastingResponder {
	var <>spec, <>message = \value, <responder;

	*new { | spec, message = \value |
		^this.newCopyArgs(spec = spec.asSpec ?? { NullSpec }, message);
	}

	osc_ { | path, srcID, recvPort, argTemplate, dispatcher |
		this.responder = OSCFunc({ | msg |
			this.changed(message, spec.unmap(msg[1]));
		}, path, srcID, recvPort, argTemplate, dispatcher)
		.permanent_(true);
	}

	responder_ { | argResponder |
		responder !? { responder.free };
		responder = argResponder;
	}

	midi_ { | func, msgNum, chan, msgType, srcID, argTemplate, dispatcher |
		func ?? func = {{ | ... args | args }};
		this.responder = MIDIFunc({ | ... args | this.changed(message, *func.(*args)) }, 
			msgNum, chan, msgType, srcID, argTemplate, dispatcher)
		.permanent_(true);
	}
	
	free {
		responder !? { responder.free};
		this.objectClosed; // also removes from Library.
	}

	addListener { | listener, action |
		action ?? { action = { | value | listener.mapSet(value) } };
		listener.addNotifier(this, message, action);
	}

	removeListener { | listener |
		listener.removeNotifier(this, message);
	}

	=> { | st param = \amp |
		this.addListener(st.asSynthTree.getParam(param));
	}

	>| { | st param = \amp |
		this.removeListener(st.asSynthTree.getParam(param));
	}
}

+ Symbol { 
	osc_ { | spec, path, srcID, recvPort, argTemplate, dispatcher |
		path = path ? this;
		^Registry('OSCFuncs', this, {
			BroadcastingResponder(spec)
		}).osc_(path, srcID, recvPort, argTemplate, dispatcher);
	}

	osc { ^Library.at('OSCFuncs', this) }

	midi_ { | msgNum, chan, msgType, srcID, argTemplate, dispatcher |
		^Registry('MIDIFuncs', this, {
			BroadcastingResponder()
		}).midi_(msgNum, chan, msgType, srcID, argTemplate, dispatcher);
	}

	midi { ^Library.at('MIDIFuncs', this) }

	noteOn_ { | msgNum, chan, srcID, argTemplate, dispatcher |
		^this.midi_(
			{ | vel, note, chan | [vel / 127, note.midicps, chan] },
			msgNum, chan, \noteOn, srcID, argTemplate, dispatcher
		)
	}

	noteOff_ { | msgNum, chan, srcID, argTemplate, dispatcher |
		^this.midi_(
			{ | vel note chan | [vel / 127, note.midicps, chan] },
			msgNum, chan, \noteOff, srcID, argTemplate, dispatcher
		)
	}

	control_ { | msgNum, chan, srcID, argTemplate, dispatcher |
		^this.midi_(
			{ | num chan | [num / 127, chan] },			
			msgNum, chan, \control, srcID, argTemplate, dispatcher
		)
	}

	touch_ { | msgNum, chan, srcID, argTemplate, dispatcher |
		^this.midi_(
			{ | num chan | [num / 127, chan] },
			msgNum, chan, \touch, srcID, argTemplate, dispatcher
		)
	}

	polytouch_ { | msgNum, chan, srcID, argTemplate, dispatcher |
		^this.midi_(
			{ | num note | [num / 127, note] },
			msgNum, chan, \polytouch, srcID, argTemplate, dispatcher
		)
	}

	bend_ { | msgNum, chan, srcID, argTemplate, dispatcher |
		^this.midi_(
			{ | num chan | [num / 16383, chan] },			// TODO: needs checking!
			msgNum, chan, \bend, srcID, argTemplate, dispatcher
		)
	}

	program_ { | msgNum, chan, srcID, argTemplate, dispatcher |
		^this.midi_(
			{ | num chan | [num, chan] },			
			msgNum, chan, \program, srcID, argTemplate, dispatcher
		)
	}
}