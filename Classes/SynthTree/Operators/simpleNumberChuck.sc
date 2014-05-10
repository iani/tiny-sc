+ SimpleNumber {
	=> { | chuckee | ^chuckee receiveNumberChuck: this }
	receivePatternChuck { | pattern |
		^PatternTask(pattern, this);
	}
	patternParams { | paramArray, adverb |
		^PatternEventPlayer(paramArray, this);
	}
	receivePatternInstrument { | patternInstrument |
		^patternInstrument.durations = this;
	}
}