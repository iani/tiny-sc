/*
Redo of EventStreamPlayer as stream with side effects, 
holding the pattern from which it is created.  

This should make it possible to embed an EventPattern in another Pattern, 
in similar way as Pdef can be embedded in a Pseq (see Pdef help entry).

IZ Mon, Apr 21 2014, 09:58 EEST

EventPattern((degree: [1, 2, 3].pseq(2), dur: 0.1)).play;

Pseq([EventPattern((degree: [1, 2, 3].pseq(2), dur: 0.1))]).play;

EventPattern((degree: (1..8).pseq(2), dur: 0.1)).play;

EventPattern

EventStreamPlayer

*/

EventPattern : Pattern {
	var <event; // contains patterns

	*new { | event | ^this.newCopyArgs(event ?? { () }) }

	asStream { ^EventStream(event) }

}

EventStream : Stream {
	var <event; // contains streams
	*new { | event |
		^super.new.initEventStream(event);
	}

	initEventStream { | inEvent |
		event = ();
		inEvent keysValuesDo: { | key, value |
			event[key] = value.asStream(this);
		}
	}

	next {
		var outEvent, outValue;
		outEvent = ();
		event keysValuesDo: { | key, value |
			outValue = value.next(this);
			if (outValue.isNil) { ^nil };
			outEvent[key] = outValue;
		};
		^outEvent;
	}
	
	/* This comes into play when we embed an EventPattern in a Stream as in: 
		Pseq([EventPattern((degree: (1..8).pseq(2)))]).play;
	*/
	embedInStream { arg inval;
		var outval;
		while {
			outval = this.next;
			outval.notNil;
		}{
			outval.yield;
		};
		nil;
	}	
}
