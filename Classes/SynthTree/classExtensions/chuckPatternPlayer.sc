+ SequenceableCollection {
	=> { | chuckee, adverb |
		^chuckee.patternParams(this, adverb)
	}

	pp { | repeats = 1, durations |
		^PatternPlayer(Pseq(this, repeats), durations ?? { Pfunc({ ~dur.next }) })
	}

	/*
	%> { | durations |
		^PatternPlayer (SynthPattern (this), durations)
	}
	*/
}

+ SimpleNumber {
	=> { | chuckee | ^chuckee receiveNumberChuck: this }
	receivePatternChuck { | pattern |
		^PatternPlayer(pattern, this);
	}
	patternParams { | paramArray, adverb |
		^PatternPlayer(paramArray, this);
	}
}

+ Pattern {
	=> { | chuckee, adverb |
		^switch (adverb,
			'd', { chuckee.asSynthTree setPatternDuration: this },
			'i', { chuckee.asSynthTree setPatternInstrument: this },
			{ chuckee.receivePatternChuck(this, adverb) }
		);
	}

	pp { | durations | 
		^PatternPlayer(this, durations ?? { Pfunc({ ~dur.next }) })
	}

	receivePatternChuck { | pattern |
		^PatternPlayer(pattern, this);
	}

	patternParams { | paramArray, adverb |
		if (adverb === 'i') {
			^PatternInstrument(PatternPlayer(paramArray), this);
		}{
			^PatternPlayer(paramArray, this);
		}

	}
}

+ Symbol {
	=> { | st | ^st.asSynthTree setPatternInstrument: this }

	patternParams { | paramArray, adverb |
		if (adverb === 'i') {
			^PatternInstrument(PatternPlayer(paramArray), this);
		}{
			/*
			^this.asSynthTree.playPattern(
				PatternInstrument(PatternPlayer(paramArray)),
				adverb === 'm' // merge players if 'm'
			);
			*/
			^this.asSynthTree.chuck(
				PatternInstrument(PatternPlayer(paramArray, Pfunc({ ~dur })))
			)
		};
	}

	receiveNumberChuck { | number |
		^this.asSynthTree setPatternDuration: number;
	}

	receivePatternChuck { | pattern |
		^this.asSynthTree chuck: pattern.asPatternInstrument;
	}

	// TODO: Implement this
	receiveAssociationChuck { | association |
		^this.asSynthTree.chuckPatternParam(association.key, association.value)
	}
}

+ Association {
	// Old implementation: only for single params
	/*
	=> { | param |
		^key pp: value => param
	}
	*/
	// New implementation: permit chucking into SynthTrees
	=> { | chuckee |
		^chuckee.receiveAssociationChuck(this);
	}
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

+ Object {
	globDur { currentEnvironment.parent[\dur] = this.asStream } 
}