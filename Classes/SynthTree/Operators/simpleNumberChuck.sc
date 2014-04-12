+ SimpleNumber {
	=> { | chuckee | ^chuckee receiveNumberChuck: this }
	receivePatternChuck { | pattern |
		^PatternPlayer(pattern, this);
	}
	patternParams { | paramArray, adverb |
		^PatternPlayer(paramArray, this);
	}
	receivePatternInstrument { | patternInstrument |
		^patternInstrument.durations = this;
	}
}