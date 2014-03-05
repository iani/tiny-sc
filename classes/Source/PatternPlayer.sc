/*
Play a pattern, as stream, in time.  

The timing of successive value requests from the stream is defined by another stream, 
that produces the dt (time intervals) to wait for the next call of "next". 

UNDER CONSTRUCTION

IZ Wed, Mar  5 2014, 10:47 EET
*/

PatternPlayer {
	var <valuePattern;
	var <durationPattern;
	var <>initialDelay = 0;
	var <>clock;
	var <source;
	var <valueStream;
	var <durationStream;
	var <task;
	var <currentValue, <currentDuration;

	*new { | values, durations |
		^this.newCopyArgs(values, durations);
	}

	values_ { | values |
		valuePattern = values.asPattern;
		valueStream = valuePattern.asStream;
	}

	durations_ { | durations |
		durationPattern = (durations ?? {{ source.pollRate }}).asPattern;
		durationStream = durationPattern.asStream;
	}
	
	makeSourceAction { | argSource |
		source = argSource;
		this.values = valuePattern;
		this.durations = durationPattern;
		task = Task {
			source.changed(\taskStarted);
			initialDelay.wait;
			source.changed(\taskLoopStarted);
			while {
				(currentValue = valueStream.next).notNil 
				and:
				{ (currentDuration = durationStream.next).notNil }
			}{
				source.changed(\value, currentValue);
				currentDuration.wait;
			};
			source.changed(\taskStopped);
		}
	}
    start { task.play(clock); }
    stop { task.stop; }
    isPlaying { ^task.isPlaying; }
}

+ Object {
	asPattern { | repeats = inf | ^Pn(this, repeats) }
}

+ Pattern {
	asPattern { /* ^this */ }
}

+ SequenceableCollection {
	asPattern { | repeats = inf | ^Pseq(this, repeats) }
}

+ Function {
	asPattern { | repeats = inf | ^Pfuncn(this, repeats) }
}

