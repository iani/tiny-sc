TaskPlayer {
	var <name, <pattern, <clock, stream, <dur;
	var <task, <counter, <count;
	var <valPattern, <valStream, <val;
	var <atEnd = false;

	*new { | name, pattern  = 1, clock |
		^Registry(TaskPlayer, name.asSymbol, {
			this.newCopyArgs(
				name,
				pattern,
				clock ?? { TempoClock.default },
			).init
		})
	}

	init {
		this.makeStream;
		this.makeTask;
		counter = Pseries(0, 1, inf).asStream;
		this.makeValStream;
	}

	pattern_ { | argPattern |
		pattern = argPattern;
		this.makeStream;
	}

	valPattern_ { | argPattern |
		valPattern = argPattern;
		this.makeValStream;
	}

	makeValStream { valStream = valPattern.asStream; }
	
	makeStream { stream = pattern.asStream }
	makeTask {
		if (task.isPlaying) { task.stop };
		task = Task({
			this.changed(\start);
			while { (dur = stream.next).notNil } {
				count = counter.next;
				val = valStream.next;
				this.changed(\beat);
				dur.wait;
			};
			atEnd = true;
			this.changed(\stop);
		}, clock);
	}

	// resumes task if stopped, issues warning if already running.
	// Restarts only if task has finished;
	play {
		// Note: using dur.isNil would lead to infinite loop with replay->playx
		if (atEnd) { this.init };
		task.play;
	} // does not restart!

	stop {
		task.stop;
		{
			this.changed(\stop);
			0.01.wait;
			this.changed(\stop); // trying to fix hanging synths for very fast patterns
		}.fork(SystemClock)
	} // pauses task. Does not reset.

	*stopAll {
		Library.at(TaskPlayer) do: _.stop;
	}

	replay { // restart from the beginning;
		task.stop;
		this.init;
		this.play;
	}
	top { ^this }
	topval { ^val }
	asTaskPlayer { ^this }
}
