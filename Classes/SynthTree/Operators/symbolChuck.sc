
+ Symbol {

	=> { | chuckee | ^this.asEdef => chuckee }

	=<> { | chuckee | ^this.st => chuckee }

	>| { | chuckee | ^chuckee.asSynthTree.getParam(this).bunMap }

	asPatternInstrument {
		^PatternInstrument(PatternTask([], Pfunc({ ~dur.next })), this);
	}

	patternParams { | paramArray | ^this.asSynthTree.chuckPatternParams(paramArray) }
	
	clearChuckPatternParams { | paramArray |
		^this.asSynthTree.clearChuckPatternParams(paramArray);
	}
	/*
	receiveNumberChuck { | number |
		^this.asSynthTree setPatternDuration: number;
	}
	*/
	receivePatternChuck { | pattern |
		^this.asSynthTree chuck: pattern.asPatternInstrument;
	}
}

+ SynthTree {
	chuckPatternParams { | paramArray | 
		template.chuckPatternParams(paramArray, this);
		if (template.pattern.isPlaying.not) {
			this.makeSynth;
			template.pattern.start;
		};
	}
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
	chuckPatternParams { | paramArray | this.set(paramArray); }
}

+ SequenceableCollection {
	asPatternInstrument { ^PatternInstrument(PatternEventPlayer(this)) }
}