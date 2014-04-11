/*

Methods needed to handle =>

New draft: Fri, Apr 11 2014, 15:30 EEST

*** Alternative 2: with *>

Overview / list of operators used in alternative 2:

1. => create or modify PatternPlayers, PatternInstruments, chuck things to SynthTrees.
2. *> chuck something to a parameter of a synthtree.
3. -> associate patterns to parameters.
4. =< send output of a synth to the input of another synth.

Details:

**** *> chucking to single parameters/aspects of current synthtree
*> is for chucking to single parameters or special aspects duration, legato, instrument of the current SynthTree.

- anything *> symbol :: chuck to parameter of current synth.  Special parameters:
  - duration :: duration of PatternInstrument
  - dur :: synonym of dur
  - legato :: legato (not a parameter of the PatternPlayer)
  - leg :: synonym of legato
  - instrument :: Instrument (of PatternInstrument)
  - instr :: synonym of Instrument

- anything *> `paramname :: chuck to duration of PatternPlayer of parameter `paramname.

**** value -> parameter chucking to single parameters/aspects of named synthtree

[100, 200].pseq -> \freq => \synthTree1

[100, 200].pseq -> \dur => \synthTree1

**** anything => [not symbol, not ref]: make PatternPlayer/InstrumentPlayer
- anything => [not symbol, not ref] :: make/set duration of PatternPlayer
**** anything => ref : make / set instrument of PatternInstrument
- anything => ref :: make / set instrument of PatternInstrument
**** anything => symbol / synthtree:  Chuck to symbol, synthtree

*/

+ SequenceableCollection {
	=> { | chuckee, adverb |
		^chuckee.patternParams(this, adverb)
	}

	/*
	%> { | durations |
		^PatternPlayer (SynthPattern (this), durations)
	}
	*/
}

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

+ Symbol {

	=> { | st | ^st.asSynthTree setPatternInstrument: this }

	asPatternInstrument {
		^[freq: 440] =>.i this;
	}
	
	legato_ { | legato = 1 | ^this.asSynthTree.legato = legato }

	patternParams { | paramArray, adverb |
		if (adverb === 'i') {
			^PatternInstrument(PatternPlayer(paramArray), this);
		}{
			/*
			^this.asSynthTree.playPattern(
				PatternInstrument(PatternPlayer(paramArray)),
				adverb === 'm' // merge players if 'm'
			);
			*/
			^this.asSynthTree.chuck(
				PatternInstrument(PatternPlayer(paramArray, Pfunc({ ~dur })))
			)
		};
	}

	receiveNumberChuck { | number |
		^this.asSynthTree setPatternDuration: number;
	}

	receivePatternChuck { | pattern |
		^this.asSynthTree chuck: pattern.asPatternInstrument;
	}
}

+ Object {
	globDur { currentEnvironment.parent[\dur] = this.asStream } 
}