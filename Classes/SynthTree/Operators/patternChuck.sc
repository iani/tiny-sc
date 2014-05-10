+ Pattern {
	=> { | chuckee, adverb |
		^switch (adverb,
			'd', { chuckee.asSynthTree setPatternDuration: this },
			'i', { chuckee.asSynthTree setPatternInstrument: this },
			{ chuckee.receivePatternChuck(this, adverb) }
		);
	}

	receivePatternChuck { | pattern |
		^PatternTask(pattern, this);
	}

	receivePatternInstrument { | patternInstrument |
		^patternInstrument.durations = this;
	}

	patternParams { | paramArray, adverb |
		if (adverb === 'i') {
			^PatternInstrument(PatternEventPlayer(paramArray), this);
		}{
			^PatternEventPlayer(paramArray, this);
		}
	}

	asPatternTask {
		// TODO: Test this
		^PatternTask(this);
	}
}

+ PatternTask { asPatternTask { ^this } }