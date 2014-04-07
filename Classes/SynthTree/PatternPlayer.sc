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
		.values_(values).durations_(durations ?? { Pfunc({ ~dur })});
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
	
	legato_ { | argLegato |
		valuePattern.legato = argLegato;
		valueStream.legato = argLegato;
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

	=> { | chuckee, adverb |
		^chuckee.playPattern(this)
	}

	chuckParam { | param, pattern |
		this.set(param, pattern);
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
