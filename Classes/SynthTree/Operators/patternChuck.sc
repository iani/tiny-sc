+ Pattern {
	=> { | chuckee, adverb |
		^switch (adverb,
			'd', { chuckee.asSynthTree setPatternDuration: this },
			'i', { chuckee.asSynthTree setPatternInstrument: this },
			{ chuckee.receivePatternChuck(this, adverb) }
		);
	}

	receivePatternChuck { | pattern |
		^PatternPlayer(pattern, this);
	}

	receivePatternInstrument { | patternInstrument |
		^patternInstrument.durations = this;
	}

	patternParams { | paramArray, adverb |
		if (adverb === 'i') {
			^PatternInstrument(PatternPlayer(paramArray), this);
		}{
			^PatternPlayer(paramArray, this);
		}
	}

	asPatternPlayer {
		// TODO: Test this
		^PatternPlayer(this);
	}
}