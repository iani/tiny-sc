
+ Symbol {

	=> { | st | ^st.asSynthTree setPatternInstrument: this }

	asPatternInstrument {
		^PatternInstrument(PatternPlayer([], Pfunc({ ~dur.next })), this);
	}

	patternParams { | paramArray, adverb |
		if (adverb === 'i') {
			^PatternInstrument(PatternEventPlayer(paramArray), this);
		}{
			^this.asSynthTree.chuckPatternParams(paramArray)
		};
	}

	receiveNumberChuck { | number |
		^this.asSynthTree setPatternDuration: number;
	}

	receivePatternChuck { | pattern |
		^this.asSynthTree chuck: pattern.asPatternInstrument;
	}
}

+ SynthTree {

	chuckPatternParams { | paramArray |
		template.chuckPatternParams(paramArray, this)
	}
}

+ Object {
	/* this is not a PatternInstrument, therefore  
	make a new PatternInstrument and chuck it to my SynthTree */
	chuckPatternParams { | paramArray, synthTree |
		synthTree.chuck(paramArray.asPatternInstrument(\default))
	}
}

+ SynthDef {
	chuckPatternParams { | paramArray, synthTree |
		synthTree.chuck(paramArray.asPatternInstrument(name.asSymbol))
	}
}


+ PatternInstrument {
	chuckPatternParams { | paramArray |
		paramArray keysValuesDo: { | key, value |
			pattern.set(key, value)
		}
	}
}

+ SequenceableCollection {
	asPatternInstrument { | instrument |
		^PatternInstrument(
			PatternEventPlayer(this, Pfunc({ ~dur.next })), instrument)
	}
}