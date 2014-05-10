/*

Here all methods that are needed to handle -> and *>

*/

+ Association {
	=> { | symbolOrSynthTree |
		^symbolOrSynthTree.asSynthTree(true, \default).chuckIntoParameter(value, key);
	}
}

+ Object {
	*> { | chuckee |
		^chuckee.starChuck(this)
	}
}

+ Symbol {
	starChuck { | object |
		// \default => SynthTree.newName;
		^(~st ?? { \default => SynthTree.newName; }).chuckIntoParameter(this, object);
	}
}



+ Ref {
	starChuck { | object |
		^~st.setParamPatternDuration(value, object);
	}
}

+ Event {
	*> { | st |
		var durations, paramPatterns;
		st = st.asSynthTree;
		this keysValuesDo: { | key value |
			if (key === \dur) { 
				durations = value;
			}{
				paramPatterns = paramPatterns ++ [key, value]; 
			};
		};
		paramPatterns keysValuesDo: { | parameter, pattern |
			st.asSynthTree.chuckIntoParameter(
				parameter,
				PatternTask(pattern, durations),
			);
		}
	}
}

+ SynthTree {
	chuckIntoParameter { | paramName, pattern |
		template.chuckPattern(paramName, pattern, this);
		if (this.isPlaying.not) { this.start }; // really???
	}

	setParamPatternDuration { | param, pattern |
		args.getParam(param).getPattern.durations = pattern;
	}

	getParam { | param | ^args.getParam(param) }
}

+ Object {
	// FunctionSynthTemplate, SynthDef use this:
	chuckPattern { | param, pattern, synthTree |
		synthTree.getParam(param).playPattern(pattern.asPatternTask);
	}
}

+ PatternInstrument {
	chuckPattern { | param, argPattern | 
		switch (param,
			\dur, { this.durations = argPattern },
			\duration, { this.durations = argPattern},
			// \leg, { this.legato = argPattern },
			// \legato, { this.legato = argPattern },
			// \instr, { this.instrument = argPattern },
			// \instrument, { this.instrument = argPattern },
			{ this.set([param, argPattern]); }
		);
	}
}

+ Function {
	asPatternTask { | durations = 1 | ^PatternTask(Pfunc(this), durations) }
}

+ Integer {
	asPatternTask { | durations = 1 | ^[this] asPatternTask: durations }
}

+ SequenceableCollection  { 
	asPatternTask { | durations = 1 | ^PatternTask(this.pseq, durations) }
}
