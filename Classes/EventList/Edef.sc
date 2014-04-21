/*
Redoing Mdef to play with EventPattern.

IZ Tue, Apr 22 2014, 00:42 EEST

Edef: To propagate, or not?  On demand only?


Operators? 

	\edef =< event; // add event contents to edef, do not propagate
	\edef =!< event; // replace old event contents by new event, do not propagate
	\edef =<< event; // add event contents and propagate
	\edef =!<< event; // replace event contents and propagate

The above may be chained with a chuck to a synthtree: 

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

	{ function } =>> \synthtree // produce new pattern from existing pattern
	// and substitute new pattern in stream.
	// Stream's event is modified

	event =%> \synthtree // add filter - without altering contents of event stream
	This is for playing multiple synthrees with one stream, while varying the way of playing
	on a synthree basis.
	// Stream's event is not modified.

	\edef => `estream => \synthree // play named stream (Bdef) into synthree
	// name is explicitly given by `estream ref, and overrides synthree name.



*/

Edef {
	classvar <all;
	var <>eventPattern;

	*initClass { all = IdentityDictionary() }

	*new { | name, argPattern, propagate = false |
		var instance;
		instance = all[name];
		if (instance.isNil) {
			instance = this.newCopyArgs(name, EventPattern());
		}{
			
		};
		argPattern !? { instance.clearSet(argPattern, propagate) };
		^instance;
	}

	clearSet { | argPattern, propagate = false |
		eventPattern = EventPattern(argPattern);
		if (propagate) { this.propagate };
	}

	

}