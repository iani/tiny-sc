TaskPlayer {
	var <name, <pattern, <clock, stream, <dur;
	var <task, <counter, <count;
	var <valPattern, <valStream, <val;

	*new { | name, pattern  = 1, clock |
		^Registry(TaskPlayer, name, {
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
			this.changed(\stop);
		}, clock);
	}

	// resumes task if stopped, issues warning if already running
	play { task.play; } // does not restart!

	stop { task.stop } // pauses task. Does not reset.

	replay { // restart from the beginning;
		task.stop;
		this.init;
		this.play;
	}
	top { ^this }
	topval { ^val }
}
