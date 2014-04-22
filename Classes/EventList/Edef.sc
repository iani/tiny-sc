/*
Edef: Associate an EventPattern with a symbol and implement propagation 
of later modifications of the pattern to streams played from it. 

IZ Tue, Apr 22 2014, 00:42 EEST

*/

Edef { // NamedEventPattern
	classvar <all; // TODO: Use Library instead of this, with NameSpace class
	var <name;     // Can be nil
	var <eventPattern;
	var <children;

	*initClass { all = IdentityDictionary() }

	*new { | name, argPattern, propagate = false |
		var instance;
		instance = NameSpace(\Edef, name, { this.newCopyArgs(name, EventPattern()) });
		argPattern !? { instance.replace(argPattern, propagate) };
		^instance;
	}

	replace { | argPattern, propagate = false |
		eventPattern =  argPattern.asPattern; // event returns:  EventPattern(argPattern);
		if (propagate) { this.propagate };
	}

	merge { | argPattern, propagate = false |
		eventPattern = eventPattern.merge(argPattern);
		if (propagate) { this.propagate };
	}

	propagate {
		children do: _.inherit(eventPattern);
	}

	play { | name |
		/* Creates Idef */
		var player;
		player = Idef(name, this);
		children = children add: player;
		^player;
	}

	=> { | chuckee |
		// play into named Idef (if chuckee is Ref or Idef)
		// or into SynthTree (if chuckee is Symbol or SynthTree)
		^chuckee.receiveEdef(this);
	}

	=>> { | symbol |
		// clone into Cdef named after symbol
		this.clone(symbol);
	}

	clone { | name | ^Cdef(name, this) }

}

Cdef : Edef { // NamedEventPatternClone
	// clone of an Edef.  Inherits changes propagated by parent
	var <parent; // only for removing from parent upon request
	var <mods; // locally modified elements: apply these on inherited pattern

	*new { | name, parent | ^super.new(name, parent.eventPattern).initCdef(parent); }
	initCdef { | argParent | parent = argParent; }

	// uses new Event.merge method.  Other types of mods may 
	// do intelligent merging of patterns - as distinct from Events
	// Other types, as in: A function applied on the parent pattern.
	inherit { | argPattern |
		eventPattern = argPattern.merge(mods);
		this.propagate;
	}
	
	// TODO: Add methods for changing the EventPattern + Mods

}

/*

Idef and Bdef operate on the eventStream's event.  
Prototype: 
//:
a = EventPattern((degree: [1, 2, 3].pseq(inf))).play;
//:
a.originalStream.event[\degree] = Pbrown(-5, 5, 3, inf).asStream;
//:
a.originalStream.event[\dur] = 0.1;

*/
Idef { // NamedInheritingEventStream
	var <name;
	var <parent;
	var <children;
	var <mods; // locally modified elements: apply these on inherited pattern
	var <eventStream;
	


}

Bdef : Idef { // NamedBroadcastingEventStream

}


+ Nil { merge { | parentPattern | ^parentPattern } }
+ Event {
	merge { | parentPattern |
		^parentPattern.mergeEvent(this);
	}
	mergeEvent { | modEvent |
		
	}

	mergeFunction { | modFunction |

	}
}

+ Function {
	merge { | parentPattern |
		^parentPattern.mergeFunction(this);
	}

}

+ Pattern {
	mergeEvent {

	}

	mergeFunction {

	}
}
