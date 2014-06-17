/*
Edef: Associate an EventPattern with a symbol and implement propagation 
of later modifications of the pattern to streams played from it. 
Cdef: clone of an Edef, inherits subsequent changes to father.
Idef: subclass of EventStreamPlayer, with inheritance.
Bdef: subclass of Idef, broadcasting instead of playing

These subclass of EventPattern and Idef subclass of EventStream.  
The reason is to implement the alternative asStream and embedInStream methods 
without having to add exra wrappers in a different class to handle them.

Note: Edef plays as Idef playing or broadcasting 
depending on whether it is chucking into a SynthTree or not. 


IZ Tue, Apr 22 2014, 00:42 EEST

a = Edef(\x, (degree: 20));
b = a.play;
b.inspect;
a.inspect;
c = a.broadcast;
c addDependant: { | ... args | args.postln; };
c.inspect;
delta

*/

Edef : EventPattern { // NamedEventPattern
	var <name;     // Can be nil
	var <children;

	//	*initClass { all = IdentityDictionary() }

	*new { | name, argPattern, propagate = true |
		var instance;
		instance = NameSpace(\Edef, name, { this.newCopyArgs((degree: \rest), name, Set()) });
		argPattern !? { instance.replace(argPattern, propagate) };
		^instance;
	}

	replace { | argPattern, propagate = false |
		event = argPattern;
		if (propagate) { this.propagate };
	}

	merge { | argPattern, propagate = false |
		// eventPattern = eventPattern.merge(argPattern);
		if (propagate) { this.propagate };
	}

	propagate { children do: _.inherit(event) }

	broadcast { | name | ^this.play(name, true) }

	play { | argName, broadcast = false |
		/* If broadcast, make Bdef, else make Idef+play it */
		var player;
		// TODO: Check this. Bdef/Idef new not correct - uses same stream
		argName ?? { argName = name };
		player = if (broadcast) { Bdef(argName, this) } { Idef(argName, this).play };
		// children = children add: player;
		^player
	}

	addChild { | child | children = children add: child }

	asEventStreamPlayer { | argName |
		// For playing in Ndef
		^Idef(argName, this);
	}

	=> { | chuckee |
		// play into named Idef (if chuckee is Ref or Idef)
		// or into SynthTree (if chuckee is Symbol or SynthTree)
		^chuckee.receiveEdef(this)
	}

	=>> { | symbol |
		// clone into Cdef named after symbol
		this.clone(symbol)
	}

	=< { | inEvent |
		this.addEvent(inEvent, true);
	}
	=!< { | inEvent | this.replaceEvent(inEvent, true) }
	=<| { | inEvent | this.addEvent(inEvent, false) }
	=!<| { | inEvent | this.replaceEvent(inEvent, false) }

	addEvent { | inEvent, propagate = true |
		if (children.size == 0) { this => name.asSynthTree };
		inEvent keysValuesDo: { | key value | event[key] = value };
		if (propagate) { this.propagate }
	}

	replaceEvent { | inEvent, propagate = true |
		event = inEvent;
		if (propagate) { this.propagate }
	}

	clone { | name | ^Cdef(name, this) }

	stopAll { children do: _.stop }
	asEdef { ^this }
}

Cdef : Edef { // NamedEventPatternClone
	// clone of an Edef.  Inherits changes propagated by parent
	var <parent; // only for removing from parent upon request
	var <mods; // locally modified elements: apply these on inherited pattern

	*new { | name, parent | ^super.new(name, parent.eventPattern).parent_(parent); }

	parent_ { | argParent |
		parent = argParent;
		parent.addChild(this);
	}

	// uses new Event.merge method.  Other types of mods may 
	// do intelligent merging of patterns - as distinct from Events
	// Other types, as in: A function applied on the parent pattern.
	inherit { | argPattern |
		//	eventPattern = argPattern.merge(mods);
		this.propagate;
	}
	
	// TODO: Add methods for changing the EventPattern + Mods

}

+ Event {
	chuckInto { | object | ^object.asEdef.addEvent(this, true) }
	=> { | chuckee | ^chuckee.chuckEvent(this);	}
	+> { | chuckee adverb | ^chuckee.addPlayerMods(this, adverb); }
	+!> { | chuckee | ^chuckee.replacePlayerMods(this);	}
	%> { | chuckee | ^chuckee.addMods(this); }
	%!> { | chuckee | ^chuckee.replaceMods(this); }

	merge { | parentPattern |
		^parentPattern.mergeEvent(this);
	}
	mergeEvent { | modEvent |
		
	}

	mergeFunction { | modFunction |

	}
}

+ SynthTree {
	receiveEdef { | edef | this.chuck(BdefInstrument(edef.broadcast)) }
	chuckEvent { | event |
		// this always cross-fades.
		this.chuck(BdefInstrument(Bdef.fromEvent(event)));
	}
	cloneInto { | chuckee |
		^chuckee.chuck(
			BdefInstrument(
				template.stopBdefOnSynthEnd_(false).bdef
			).stopBdefOnSynthEnd_(false)
		);
	}

	chuckInto { | chuckee, inputName = \in |
		chuckee.asSynthTree.addInputSynth(this, inputName);
	}

	addEvent { | event | template.addEvent(event); }
	replaceEvent { | event | template.replaceEvent(event); }
	addMods { | event | template.addMods(event); }
	replaceMods { | event | template.replaceMods(event); }
	addPlayerMods { | object, adverb |
		object.add2SynthTree(this, adverb)
	}
	replacePlayerMods { | event | template.replacePlayerMods(event); }
}

+ BdefInstrument {
	chuckEvent { | event |
		bdef.chuckEvent(event);
	}
}

+ Symbol {
	receiveEdef { | edef | ^this.asSynthTree.chuck(BdefInstrument(edef.broadcast)) }
	chuckEvent { | event | ^this.asSynthTree.chuckEvent(event); }
	addEvent { | event | ^this.asSynthTree.addEvent(event); }
	replaceEvent { | event | ^this.asSynthTree.replaceEvent(event); }
	addMods { | event | ^this.asSynthTree.addMods(event); }
	replaceMods { | event | ^this.asSynthTree.replaceMods(event); }
	addPlayerMods { | event | ^this.asSynthTree.addPlayerMods(event); }
	replacePlayerMods { | event | ^this.asSynthTree.replacePlayerMods(event); }

	=< { | chucker | ^chucker chuckInto: this }
	chuckInto { | object | object.asSynthTree addInputSynth: this.asSynthTree }

	=!< { | event | this.asEdef =!< event}
	=<| { | event | this.asEdef =<| event}
	=!<| { | event | this.asEdef =!<| event}
	=>> { | symbol | this.asEdef =>> symbol }
	asEdef { ^Edef(this) }
	asIdef { | edef | ^Idef(this, edef.asEdef) }

	cloneInto { | chuckee |
		^this.asSynthTree cloneInto: chuckee.asSynthTree;
	}
}

+ Ref {
	receiveEdef { | edef | ^value.asIdef(edef.asEdef).play; }
}

+ Nil { merge { | parentPattern | ^parentPattern } }

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
