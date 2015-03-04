+ SequenceableCollection {
	=> { | chuckee, adverb |
		^chuckee.patternParams(this, adverb)
	}

	=-> { | chuckee |
		^chuckee.clearChuckPatternParams(this)
	}
}