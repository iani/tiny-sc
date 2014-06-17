+ SimpleNumber {

	receivePatternChuck { | pattern |
		^PatternTask(pattern, this);
	}
	patternParams { | paramArray, adverb |
		^PatternEventPlayer(paramArray, this);
	}
	receivePatternInstrument { | patternInstrument |
		^patternInstrument.durations = this;
	}

	+> { | chuckee paramName |
		^chuckee.asSynthTree.set(paramName, this);
	}

}