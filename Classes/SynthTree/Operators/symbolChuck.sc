
+ Symbol {

	=> { | st | ^st.asSynthTree setPatternInstrument: this }

	asPatternInstrument {
		^[freq: 440] =>.i this;
	}
	
	legato_ { | legato = 1 | ^this.asSynthTree.legato = legato }

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
		synthTree.chuck(
			paramArray.asPatternInstrument(if (this.isNil) { \default }, { this.name })
		)
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
		^PatternInstrument(PatternPlayer(this, Pfunc({ ~dur.next })), instrument)
	}
}