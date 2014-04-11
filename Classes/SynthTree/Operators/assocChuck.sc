/*

Here all methods that are needed to handle ->

*/

+ Association {
	=> { | chuckee | ^chuckee.receiveAssociationChuck(this); }
}

+ MultiControl {
	receiveAssociationChuck { | association |
		^association.key.pp(association.value) => this;
	}
}

+ Function {
	pp { | durations | 
		^PatternPlayer(Pfunc(this), durations ?? { Pfunc({ ~dur.next }) })
	}

	ppn { | repeats = 1, durations | 
		^PatternPlayer(Pfuncn(this, repeats), durations ?? { Pfunc({ ~dur.next }) })
	}
}

+ Pattern {
	pp { | durations | 
		^PatternPlayer(this, durations ?? { Pfunc({ ~dur.next }) })
	}
}

+ SequenceableCollection {

	pp { | repeats = 1, durations |
		^PatternPlayer(Pseq(this, repeats), durations ?? { Pfunc({ ~dur.next }) })
	}

}


+ Symbol {
	receiveAssociationChuck { | association |
		^this.asSynthTree.chuckPatternParam(association.key, association.value)
	}
}

+ SynthTree {
	receiveAssociationChuck { | association |
		this.chuckPatternParam(association.key, association.value);
	}

	chuckPatternParam { | param, pattern |
		template.pattern.chuckParam(param, pattern);
		if (this.isPlaying.not) { this.start }; // really???
	}
}

+ PatternInstrument {
	chuckParam { | param, pattern | pattern.chuckParam(param, pattern) }
}

/*
+ Pattern {
	chuckParam { | param, pattern | this.set(param, pattern); }
}
*/