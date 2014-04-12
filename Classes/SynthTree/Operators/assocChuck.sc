/*

Here all methods that are needed to handle ->

*/

+ Association {
	=> { | symbolOrSynthTree |
		^symbolOrSynthTree.asSynthTree.chuckIntoParameter(this.value, this.key);
	}
}

+ SynthTree {
	chuckIntoParameter { | paramName, pattern |
		switch (paramName,
			\dur, { template.durations = pattern },
			\duration, { template.durations = pattern},
			\leg, { template.legato = pattern },
			\legato, { template.legato = pattern },
			\instr, { template.instrument = pattern },
			\instrument, { template.instrument = pattern },
			{ template.chuckPattern(paramName, pattern, this); }
		);
		if (this.isPlaying.not) { this.start }; // really???
	}
}

+ Object {
	chuckPattern { | param, pattern, synthTree |
		syntThree.getParam(param).playPattern(pattern);
	}
}

+ PatternInstrument {
	chuckPattern { | param, argPattern | pattern.set(param, pattern) }
}
