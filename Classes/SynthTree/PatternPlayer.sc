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

	set { | values |
		this.values_(values); // PatternEventPlayer overrides this
	}

	values_ { | values |
		valuePattern = values;
		valueStream = valuePattern.asStream;
	}

	durations_ { | durations |
		durations ?? { durations = durationPattern ? 1 };
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
	=> { | chuckee, adverb | ^chuckee.playPattern(this) }
}

PatternEventPlayer : PatternPlayer {

	*new { | values, delay = 0, clock |
		^this.newCopyArgs(delay, clock ?? { TempoClock /*() */ })
		.values_(values).initPatternEventPlayer;
	}

	values_ { | values |
		// reset pattern/stream if new values are provided
		if (values.notNil or: { valuePattern.isNil }) {
			valuePattern = SynthPattern([]);
			valueStream = valuePattern.asStream;
		};
		this.set(values);
	}

	initPatternEventPlayer {
		durationPattern ?? { this.durations = 1; }
	}

	set { | values |
		// add all key-value pairs - except for dur.
		// dur sets durationPattern/durationStream instead.
		values keysValuesDo: { | param, value |
			if (param === \dur) {
				this.durations = value;
			}{
				valuePattern.set (param, value);
				valueStream.set (param, value);
			};
		};
		this.changed(\values);
	}
}

SynthPattern {
	var <params;

	*new { | params | ^this.newCopyArgs ( params); }
	asStream { ^SynthStream(params) }

	set { | param, value |
		var index;
		index = params indexOf: param;
		if (index.isNil) {
			params = params ++ [param, value];
		}{
			params[index + 1] = value;
		}
	}

	asPatternPlayer { | durations | ^params.asPatternPlayer(durations); }
}

SynthStream {
	var <params; // , <legato;

	*new { | params /* , legato = 1 */ |
		^this.newCopyArgs(ParamStream(params) /* , legato.asStream */ );
	}
	next { | dur | ^params.asEvent(dur); }
	set { | param, pattern | params.set(param, pattern); }
}

ParamStream {
	var <keys, <values;

	*new { | params | ^super.new.initParamStream(params) }

	initParamStream { | argParams |
		#keys, values = argParams.clump(2).flop;
		values = values collect: _.asStream;
	}

	asEvent { | duration |
		var event;
		event = (dur: duration);
		keys do: { | key, index | event[key] = values[index].next };
		^event;
	}

	set { | param, value |
		var index;
		index = keys indexOf: param;
		if (index.isNil) {
			keys = keys add: param;
			values = values add: value.asStream;
		}{
			values[index] = value.asStream;
		}
	}
}

// ================================================================

PatternFunc {
	var <pattern, <receiver, <>action;
	*new { | pattern, receiver, action |
		^this.newCopyArgs(pattern, receiver, action).enable;
	}

	enable { this.addNotifier(pattern, \value, action); }
	disable { this.removeNotifier(pattern, \value); }

	remove {
		this.disable;
		this.objectClosed;
	}
}