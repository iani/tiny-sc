
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
}
