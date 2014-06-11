/*
This version does not work correctly when cloning multiple Idefs from same Edef. 
Fix is underway in Idef2.  Wed, Apr 30 2014, 19:04 EEST


Idef applies inheritance/modification operations on the originalStream event.

Bdef plays inside SynthDef by broadcasting its event and letting 

Prototype: 
//:
a = EventPattern((degree: [1, 2, 3].pseq(inf))).play;
//:
a.originalStream.event[\degree] = Pbrown(-5, 5, 3, inf).asStream;
//:
a.originalStream.event[\dur] = 0.1;
//: Testing play into synthtree:

a = Idef.fromEvent((degree: [10, 12].pseq, dur: 0.2));
a.play;

a.inspect;
*/
Idef : EventStreamPlayer { // NamedInheritingEventStreamPlayer
	var <>name;
	var <parent;
	var <children;
	var <mods; // locally modified elements: apply these on inherited pattern
	
	*new { | name, parent, protoEvent |
		^NameSpace(\Idef, name, {
			super.new(parent.asStream, protoEvent).initIdef(name, parent);
		});
	}

	initIdef { | argName argParent |
		name = argName;
		this.parent = argParent;
		children = Set();
	}

	parent_ { | argParent |
		parent = argParent;
		parent !? { parent.addChild(this); };
	}

	addChild { | child | children = children add: child }

	*fromEvent { | event, protoEvent |
		^this.new(nil, Edef(nil, event), protoEvent)
	}

	addEvent { | event, fromPattern = false |
		var newEvent;
		newEvent = if (fromPattern) { 
			this.originalPatternEvent;
		}{
			originalStream.event.copy;
		};
		newEvent use: {
			event keysValuesDo: { | key value | newEvent[key] = value.value.asStream; };
		};
		this.applyMods(newEvent);
	}

	replaceEvent { | event |
		event = event.copy;
		this.applyMods(
			event keysValuesDo: { | key value | event[key] = value.asStream; }
		);
	}

	replacePlayerMods { | event |
		// replace mods with event, apply to stream from original pattern
		mods = event;
		this.applyMods(this.pattern.asStream.event);
	}

	addPlayerMods { | event, reset = false |
		var newEvent;
		mods ?? { mods = () };
		event keysValuesDo: { | key value | mods[key] = value };
		this.applyMods(if (reset) { this.originalPatternEvent }  { nil });
	}

	originalPatternEvent { ^this.pattern.asStream.event.copy }
	pattern { ^parent.pattern }

	propagate { | inEvent | children do: _.inherit(inEvent) }
	inherit { | inEvent | this.applyMods(inEvent); }

	applyMods { | inEvent |
		inEvent = (inEvent ?? { originalStream.event; }).copy;
		mods !? { 
			inEvent use: {
				mods keysValuesDo: { | key value | inEvent[key] = value.value } 
			};
		};
		inEvent keysValuesDo: { | key value | inEvent[key] = value.asStream };
		inEvent[\dur] ?? { inEvent[\dur] = 1 };
		originalStream.event = inEvent;
		this.propagate(inEvent);
	}

	asIdef { | edef |
		// ignore edef: no switching parent edefs!
		^this
	}
}

Bdef : Idef {

	init {
		if (originalStream.event.dur.isNil) { originalStream.event.dur = 1 };
		super.init;
	}

	prNext { arg inTime;
		var nextTime;
		var outEvent = stream.next(event.copy);
		if (outEvent.isNil) {
			streamHasEnded = stream.notNil;
			cleanup.clear;
			this.removedFromScheduler;
			this.changed(\taskStopped);
			^nil
		}{
			// Instead of playAndDelta, use broadcastAndDelta.
			nextTime = outEvent.broadcastAndDelta(cleanup, muteCount > 0, this);
			if (nextTime.isNil) { this.removedFromScheduler; ^nil };
			nextBeat = inTime + nextTime;	// inval is current logical beat
			^nextTime
		};
	}

}

+ Event {
	broadcastAndDelta { | cleanup, mute, streamPlayer |
		if (mute) { this.put(\type, \rest) };
		cleanup.update(this);
		// this.play;  // instead of this, use "changed".
        // instead of playing, broadcast, with "changed":
        streamPlayer.changed(\event, this);
		^this.delta;
	}
}