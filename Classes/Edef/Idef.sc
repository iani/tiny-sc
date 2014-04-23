/*

Idef applies inheritance/modification operations on the originalStream event.

Bdef plays inside SynthDef by broadcasting its event and letting 

Prototype: 
//:
a = EventPattern((degree: [1, 2, 3].pseq(inf))).play;
//:
a.originalStream.event[\degree] = Pbrown(-5, 5, 3, inf).asStream;
//:
a.originalStream.event[\dur] = 0.1;

*/
Idef : EventStreamPlayer { // NamedInheritingEventStreamPlayer
	var <>name;
	var <parent;
	var <children;
	var <mods; // locally modified elements: apply these on inherited pattern
	
	*new { | name, parent, protoEvent |
		^NameSpace(\Idef, name, { 
			super.new(parent.asStream, protoEvent).name_(name);
		});
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