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

	*new { | values, durations, delay, clock |
		^this.newCopyArgs(delay, clock)
		.values_(values).durations_(durations);
	}

	values_ { | values |
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
				(currentValue = valueStream.next).notNil 
				and:
				{ (currentDuration = durationStream.next).notNil }
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
}

/* 
IZ Sun, Mar 30 2014, 16:37 EEST
*/

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
	remove { this.disable }
}

+ SequenceableCollection {
	=> { | durations, repeats |
		repeats = repeats ? 1;
		if (repeats == 'i') { repeats = inf };
		^PatternPlayer(Pseq(this, repeats), durations);
		
	}

	pp { | repeats = 1, durations |
		^PatternPlayer(Pseq(this, repeats), durations ?? { Pfunc({ ~dur }) })
	}
}

