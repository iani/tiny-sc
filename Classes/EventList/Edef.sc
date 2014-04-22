/*
Edef: Associate an EventPattern with a symbol and implement propagation 
of later modifications of the pattern to streams played from it. 

IZ Tue, Apr 22 2014, 00:42 EEST

Operators:

\edef =< event; // create/add event contents to edef, do not propagate
\edef =!< event; // create/ replace old event contents by new event, do not propagate
\edef =<< event; // add event contents and propagate
\edef =!<< event; // replace event contents and propagate

\edef =>> \cdef; // clone edef into cdef.  cdef inherits future changes from edef.

If a function instead of an event is passed as second argument in the above, 
then the function is evaluated with ~pattern as environment variable, and the 
result becomes the new pattern of the Mdef.

Furthermore, the above may be chained with a chuck to a synthtree: 

\edef =< event => \synthtree
\edef =!< event => \synthtree
\edef =<< event => \synthtree
\edef =!<< event => \synthtree

Going directly to SynthTree:

\edef => \synthtree; // play a new stream into a synthtree
// Stream is named after synthtree.
// Stream replaces previous stream

event => \synthree // chuck event to synthree's stream and play. 
// Stream is named after synthtree.
// New stream contents replace previous contents (no merge).

event =>> \synthree // add event contents into synthree (merge).
// Stream's event is modified

{ function } =>> \synthtree // produce new stream from existing stream
// and substitute new stream in the stream player.
// Stream's event is modified

event =%> \synthtree 
{ function } =%> \synthree
add filter - without altering contents of event stream
This is for playing multiple synthrees with one stream, while varying the way of playing
on a synthree basis.
Stream's event is not modified.  Durations of the stream cannot be modified.

Using symbols references to explicitly name spawned streams:
\edef => `estream // create Bdef named `estream from edef.
\edef => `estream => \synthree // play named stream (Bdef) into synthree
// name is explicitly given by `estream ref, and overrides synthree name.

*/

/*
TODO: Write abstract superclass implementing nameing and Inheritance + Propagation.
Store named instances in Library[Class, name].

Hierarchy: 

NamedInheritor
   Edef  EventPattern holder
      Cdef  cloned EventPattern holder
   Idef  Simple EventStream holder
	  Bdef  Broadcasting EventStream holder
*/

Edef { // NamedEventPattern
	classvar <all; // TODO: Use Library instead of this, thus enabling subclassing
	var <eventPattern;
	var <inheritors;

	*initClass { all = IdentityDictionary() }

	*new { | name, argPattern, propagate = false |
		var instance;
		instance = all[name];
		if (instance.isNil) { instance = this.newCopyArgs(name, EventPattern()) };
		argPattern !? { instance.replace(argPattern, propagate) };
		^instance;
	}

	replace { | argPattern, propagate = false |
		eventPattern = EventPattern(argPattern);
		if (propagate) { this.propagate };
	}

	merge { | argPattern, propagate = false |
		[this, thisMethod.name, "not implemented"].postln;
		if (propagate) { this.propagate };
	}

	clone { | name | ^Cdef(name, this) }

	propagate {
		[this, thisMethod.name, "not implemented"].postln;
		// this.changed(eventPattern);
		inheritors do: _.propagate;
	}

	play { | name |
		/* Creates Bdef */
		var player;
		[this, thisMethod.name, "not implemented"].postln;
		player = Idef(name, this);
		inheritors = inheritors add: player;
		^player;
	}

}

Cdef : Edef { // NamedEventPatternClone
	// clone of an Edef.  Inherits changes propagated by parent
	var <parent;
	var <mods; // locally modified elements: apply these on inherited pattern

	*new { | name, parent | ^super.new(name, parent.eventPattern).initCdef(parent); }
	initCdef { | argParent | parent = argParent; }

	propagate {
		this.inherit;
		inheritors do: _.propagate;
	}

	// uses new Event.merge method.  Other types of mods may 
	// do intelligent merging of patterns - as distinct from Events
	// Other types, as in: A function applied on the parent pattern.
	inherit { eventPattern = mods.merge(parent.eventPattern); }
}

Idef { // NamedInheritingEventStream
	classvar <all;
	var <name;
	var <edef;
	var <eventStream;
	var <mods; // locally modified elements: apply these on inherited pattern
	
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
