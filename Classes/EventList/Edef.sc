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

event => `estream // modify estream 
{ function } => `estream // modify estream 

// name is explicitly given by `estream ref, and overrides synthree name.

*/

/*
TODO: Write abstract superclass implementing Inheritance + Propagation.
Elss use Inheritor as a mixin instead of as a superclass.

Also maybe use generator class NameSpace for named instances, instead of subclassing.

Hierarchy: 
Inheritor
	Edef  Named Inheriting EventPattern holder

	Following 2 Can be one class.  Difference is that the second one is made using 
	NameSpace to access by name.
	[
	Istream Inheriting EventStream holder (result of Ndef.play)
	Nstream Named Inheriting EventStream holder (result of Ndef => `\streamname)
	]
	    Bstream  Broadcasting EventStream holder (result of Ndef => \synthtree)

Modes of mods?

1. Replace
2. Modify

How they coincide and how they differ: 

- Both optionally propagate the RESULTING new pattern/stream to inheritors
- Replace / modify needs more analysis, here: 

Possible actions: 

- Replace mods
- Merge mods
- Cut inheritance???

Analysis of mod possibilities 

Possibilities 1 - 2 can coexist in any combination.
Possibilities 3 and 4 are exclusive of any other possibility.

1a. Replace a param pattern of the parent by a different pattern (possibly remove)
1b. Replace a param pattern of the parent by a pattern modifying the parent pattern
2.  Add a param pattern. 
3a. Replace the entire pattern of the parent by a different pattern.
3b. Replace the entire pattern of the parent by a different pattern derived from the parent.
4. Replace nothing.

Inheriting process: 

Inheriting takes the pattern from the parent and combines it with 
the mods to produce the pattern that will be used by the Edef/Idef that uses it.
It also propagates the resulting pattern to all inheritors. 

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
