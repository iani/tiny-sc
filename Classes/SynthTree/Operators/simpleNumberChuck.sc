+ SimpleNumber {
	=> { | chuckee | ^chuckee receiveNumberChuck: this }
	receivePatternChuck { | pattern |
		^PatternPlayer(pattern, this);
	}
	patternParams { | paramArray, adverb |
		^PatternEventPlayer(paramArray, this);
	}
	receivePatternInstrument { | patternInstrument |
		^patternInstrument.durations = this;
	}
}