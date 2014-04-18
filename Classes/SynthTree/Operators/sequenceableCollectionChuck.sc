+ SequenceableCollection {
	=> { | chuckee, adverb |
		[this, thisMethod.name, chuckee].postln;
		^chuckee.patternParams(this, adverb)
	}

	=!> { | chuckee |
		^chuckee.clearChuckPatternParams(this)
	}
}