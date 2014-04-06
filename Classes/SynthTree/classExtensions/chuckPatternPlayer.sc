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
			^this.asSynthTree.playPattern(
				PatternInstrument(PatternPlayer(paramArray)),
				adverb === 'm' // merge players if 'm'
			);
		};
	}

	receiveNumberChuck { | number |
		^this.asSynthTree setPatternDuration: number;
	}

	receivePatternChuck { | pattern |
		^this.asSynthTree chuck: pattern.asPatternInstrument;
	}
}

+ Association {
	=> { | param |
		^key pp: value => param
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