/*
Play a pattern, as stream, in time.  

The timing of successive value requests from the stream is defined by another stream, 
that produces the dt (time intervals) to wait for the next call of "next". 

UNDER CONSTRUCTION

IZ Wed, Mar  5 2014, 10:47 EET
*/

PatternPlayer {
	
	var <>initialDelay;
	var <>clock;
	var <valuePattern;
	var <durationPattern;
	var <valueStream;
	var <durationStream;
	var <task;
	var <currentValue, <currentDuration;

	*new { | values, durations, delay = 0, clock |
		^this.newCopyArgs(delay, clock ?? { TempoClock /*() */ })
		.values_(values).durations_(durations);
	}

	values_ { | values |
		if (values isKindOf: SequenceableCollection) {
			values = SynthPattern(values);
		};
		valuePattern = values;
		valueStream = valuePattern.asStream;
	}

	durations_ { | durations |
		durationPattern = durations;
		durationStream = durationPattern.asStream;
	}
	
	makeTask {
		task = Task {
			this.changed(\taskStarted);
			initialDelay !? { initialDelay.wait; };
			this.changed(\taskLoopStarted);
			while {
				(currentDuration = durationStream.next).notNil 
				and:
				{ (currentValue = valueStream next: currentDuration).notNil }
			}{
				this.changed(\value, currentValue);
				currentDuration.wait;
			};
			this.changed(\taskStopped);
			task = nil;
		}
	}
    start {
		task ?? { this.makeTask }; 
		task.play(clock);
	}

    stop { task.stop; }
	reset { task.reset; }

    isPlaying { ^task.isPlaying; }

	=> { | chuckee |
		chuckee.playPattern(this)
	}

	set { | param, value |
		/* When playing a patternplayer as synth in a synthtree */
		this.setPatternParam (param, value);
		this.setStreamParam (param, value);
	}

	setPatternParam { | param, value |
		valuePattern.set (param, value);
	}

	setStreamParam { | param, value |
		valueStream.set (param, value);
	}
}

PatternFunc {
	var <pattern, <receiver, <>action;
	*new { | pattern, receiver, action |
		^this.newCopyArgs(pattern, receiver, action).enable;
	}

	enable {
		this.addNotifier(pattern, \value, action);
	}

	disable {
		this.removeNotifier(pattern, \value);
	}
	remove {
		this.disable;
		this.objectClosed;
	}
}

+ Object {
	playPattern { | patternPlayer, action, startNow = true |
		var stream, pfunc;
		stream = this.asStream;
		action ?? { 
			var synth;
			action = { | value |
				synth = Synth (value.instrument, value.params);
				SystemClock.sched (value.dur, { synth.release });
			};
		};
		pfunc = PatternFunc (patternPlayer, this, action);
		if (startNow and: { patternPlayer.isPlaying.not }) { patternPlayer.start };
		^pfunc;
	}
}

+ Symbol {
	playPattern { | patternPlayer, action, startNow = true |
		var pfunc;
		action ?? { 
			action = { | value |
				var synth;
				synth = Synth (this, value.params);
				SystemClock.sched (value.dur, { synth.release });
			};
		};
		pfunc = PatternFunc (patternPlayer, this, action);
		if (startNow and: { patternPlayer.isPlaying.not }) { patternPlayer.start };
		^pfunc;
	}
}

+ Pattern {
	playPattern { | patternPlayer, action, startNow = true |
		^this.asStream.playPattern (patternPlayer, action, startNow)
	}
}

+ Routine {
	playPattern { | patternPlayer, action, startNow = true |
		var pfunc;
		action ?? { 
			action = { | value |
				var next, synth;
				next = this.next (value);
				if (next.notNil) {
					synth = Synth (next, value.params);
					SystemClock.sched (value.dur, { synth.release })
				};
			};
		};
		pfunc = PatternFunc (patternPlayer, this, action);
		if (startNow and: { patternPlayer.isPlaying.not }) { patternPlayer.start };
		^pfunc;
	}
}

/*
	// TODO: 
	// 1. Fix synth order actions
	// 2. possibly create group to be handled as synth of SynthTree. for fadein/out
+ SynthTree {
	playPattern { | patternPlayer, action, startNow = true |
		var pfunc;
		action ?? { 
			action = { | value |
				var instr;
				instr = value.instrument;
				if (instr.notNil) {
					synth = Synth (instr, value.params ++ this.ioArgs, this.group,
					addAction: \addToHead);
					SystemClock.sched (value.dur, { synth.release })
				};
			};
		};
		pfunc = PatternFunc (patternPlayer, this, action);
		if (startNow and: { patternPlayer.isPlaying.not }) { patternPlayer.start };
		^pfunc;
	}
}
*/

+ SequenceableCollection {
	=> { | chuckee, adverb |
		^chuckee.patternParams(this, adverb)
	}

	pp { | repeats = 1, durations |
		^PatternPlayer(Pseq(this, repeats), durations ?? { Pfunc({ ~dur.next }) })
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
	/*
+ SimpleNumber {
	=> { | param | param.set(this); }
}
*/
	patternParams { | paramArray, adverb |
		^PatternInstrument(PatternPlayer(paramArray, this));
	}
}

+ Pattern {
	/*
	=> { | st, adverb |
		^switch (adverb,
			'd', { st.asSynthTree setPatternDuration: this },
			'i', { st.asSynthTree setPatternInstrument: this },
		)
	}

+ Pattern {
	=> { | durations |
		^this pp: durations;
	}
	
}
	*/

	=> { | chuckee, adverb |
		^switch (adverb,
			'd', { chuckee.asSynthTree setPatternDuration: this },
			'i', { chuckee.asSynthTree setPatternInstrument: this },
			{ chuckee.receivePatternChuck(this, adverb) }
		);
	}

	pp { | durations | 
		^PatternPlayer(this, durations ?? { Pfunc({ ~dur.next }) })
	}

	receivePatternChuck { | pattern |
		^PatternPlayer(pattern, this);
	}

	patternParams { | paramArray, adverb |
		if (adverb === 'i') {
			^PatternInstrument(PatternPlayer(paramArray), this);
		}{
			^PatternInstrument(PatternPlayer(paramArray, this));
		}

	}
}

+ Symbol {
	=> { | st | ^st.asSynthTree setPatternInstrument: this }

	patternParams { | paramArray, adverb |
		if (adverb === 'i') {
			^PatternInstrument(PatternPlayer(paramArray), this);
		}{
			^this.asSynthTree.playPattern(
				PatternInstrument(PatternPlayer(paramArray)),
				adverb === 'm' // merge players if 'm'
			);
		};
	}

	receiveNumberChuck { | number |
		^this.asSynthTree setPatternDuration: number;
	}

	receivePatternChuck { | pattern |
		^this.asSynthTree chuck: pattern.asPatternInstrument;
	}
}

+ Ref {
	patternParams { | paramArray, adverb |
		//		^PatternInstrument(PatternPlayer(paramArray, this));
	}
}

+ PatternInstrument {
	
}

+ Association {
	=> { | param |
		^key pp: value => param
	}
}

+ Function {
	pp { | durations | 
		^PatternPlayer(Pfunc(this), durations ?? { Pfunc({ ~dur.next }) })
	}
	ppn { | repeats = 1, durations | 
		^PatternPlayer(Pfuncn(this, repeats), durations ?? { Pfunc({ ~dur.next }) })
	}
}

+ Object {
	globDur { currentEnvironment.parent[\dur] = this.asStream } 
}