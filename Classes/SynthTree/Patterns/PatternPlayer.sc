/*
Play a pattern, as stream, in time.  

The timing of successive value requests from the stream is defined by another stream, 
that produces the dt (time intervals) to wait for the next call of "next". 

UNDER CONSTRUCTION

IZ Wed, Mar  5 2014, 10:47 EET
*/

PatternTask {
	
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
    start { | synchOnBeat = true |
		if (this.isPlaying.not) {
			task ?? { this.makeTask };
			if (synchOnBeat) {
				clock.schedAbs(clock.beats.ceil, { task.play(clock) });
			}{
				task.play(clock)
			} 
		};
	}
    stop { task.stop; }
	reset { task.reset; }
    isPlaying { ^task.isPlaying; }
	=> { | chuckee, adverb | ^chuckee.playPattern(this) }
	monitor { | active = true |
		if (active) {
			this.addNotifier(this, \value, { | val | [this, val].postln; })
		}{
			this.removeNotifier(this, \value)
		};
	}
}

PatternEventPlayer : PatternTask {

	*new { | values, delay = 0, clock |
		^this.newCopyArgs(delay, clock ?? { TempoClock /*() */ })
		.values_(values).initPatternEventPlayer;
	}

	inheritValues { | values |
		/*  values set by Mdef. 
			Change only patterns that are not identical, initializing their Streams.
			Do not notify changed since this comes from Mdef. */
		values !? {
			this.keepOnly(values.keys.asArray);
			this.set(values, false)
		};
	}

	keepOnly { | keys |
		valuePattern.keepOnly(keys);
		valueStream.keepOnly(keys);
	}

	clear { this.values = [] }

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

	set { | values, notify = true |
		// add all key-value pairs - except for dur.
		// dur sets durationPattern/durationStream instead.
		values ?? { [] } keysValuesDo: { | param, value |
			if (param === \dur) {
				this.durations = value;
			}{
				valuePattern.set (param, value, valueStream);
			};
		};
		if (notify) {this.changed(\values, values) };
	}
}

SynthPattern {
	var <params;

	*new { | params | ^this.newCopyArgs (params ?? { [] }); }
	asStream { ^SynthStream(params) }

	set { | param, value, stream |
		var index;
		// Only change streams if the patterns are also changed:
		// Do not reset running streams if their pattern has not changed!
		params ?? { params = [] };
		index = params indexOf: param;
		if (index.isNil) {
			params = params ++ [param, value];
			stream.set(param, value);
		}{
			if (params[index + 1] !== value) {
				params[index + 1] = value;
				stream.set(param, value);
			}
		}

	}

	keepOnly { | keysToKeep |
		var keys, values, newParams;
		#keys, values = params.clump(2).flop;
		params keysValuesDo: { | key value |
			if (keysToKeep includes: key) { newParams = newParams ++ [key, value] };
		};
		params = newParams;
	}

	asPatternTask { | durations | ^params.asPatternTask(durations); }
}

SynthStream {
	var <params;

	*new { | params |
		^this.newCopyArgs(ParamStream(params));
	}
	next { | dur | ^params.asEvent(dur); }
	set { | param, pattern | params.set(param, pattern); }
	keepOnly { | keys | params.keepOnly(keys) }
}

ParamStream {
	var <keys, <values;

	*new { | params | ^super.new.initParamStream(params) }

	initParamStream { | argParams |
		#keys, values = argParams.clump(2).flop;
		values = values collect: _.asStream;
	}

	asEvent { | duration |
		var event, nextValue; // , sawNil = true;
		if (keys.size == 0) { ^nil };
		event = (dur: duration);
		keys do: { | key, index |
			nextValue = values[index].next;
			if (/* sawNil = */ nextValue.isNil) { ^nil };
			event[key] = nextValue
		};
		//		^if (sawNil) { ^nil } { ^event };
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

	keepOnly { | keysToKeep |
		var index, newKeys, newValues;
		keysToKeep do: { | key |
			index = keys indexOf: key;
			if (index.notNil) {
				newKeys = newKeys add: key;
				newValues = newValues add: values[index];
			};
		};
		keys = newKeys ?? { [] };
		values = newValues ?? { [] };
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