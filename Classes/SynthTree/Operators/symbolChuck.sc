
+ Symbol {

	=> { | params | ^this.asMdef set: params }

	asPatternInstrument {
		^PatternInstrument(PatternPlayer([], Pfunc({ ~dur.next })), this);
	}

	patternParams { | paramArray | ^this.asSynthTree.chuckPatternParams(paramArray) }
	
	clearChuckPatternParams { | paramArray |
		^this.asSynthTree.clear.patternParams(paramArray);
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
				[this, thisMethod.name, paramArray].postln;
		template.chuckPatternParams(paramArray, this) }
}

+ Object {
	/* this is not a PatternInstrument, therefore  
	make a new PatternInstrument and chuck it to my SynthTree */
	chuckPatternParams { | paramArray, synthTree |
		synthTree.chuck(paramArray.asPatternInstrument(\default))
	}
}

+ SynthDef {
	chuckPatternParams { | paramArray, synthTree |
				[this, thisMethod.name, paramArray].postln;
		synthTree.chuck(paramArray.asPatternInstrument(name.asSymbol))
	}
}


+ PatternInstrument {
	chuckPatternParams { | paramArray | this.set(paramArray) }
}

+ SequenceableCollection {
	asPatternInstrument { ^PatternInstrument(PatternEventPlayer(this)) }
}