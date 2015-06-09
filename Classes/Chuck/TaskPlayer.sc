TaskPlayer {
	var <name, <pattern, <clock, stream, <dur;
	var <task;

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
	}

	pattern_ { | argPattern |
		pattern = argPattern;
		this.makeStream;
	}
	
	makeStream { stream = pattern.asStream }
	makeTask {
		if (task.isPlaying) { task.stop };
		task = Task({
			this.changed(\start);
			while { (dur = stream.next).notNil } {
				this.changed(\beat, dur);
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
	
}

+ Chuck {
	addToTask { | task, filter |
		^filter.connectChuckTask(this, task);
	}
	
	removeFromTask { | task |
		// this.release; // called by other methods higher-up in the call chain?
		this.removeNotifier(task, \beat);
		this.removeNotifier(task, \stop);
	}
}

+ Nil {
	connectChuckTask { | chuck, task |
		chuck.addNotifier(task, \beat, { | dur | chuck.play(dur) });
		chuck.addNotifier(task, \stop, { chuck.release });
		^task;
	}
}

+ Object {
	*> { | chuckName |
		^Chuck(chuckName).addToTask(TaskPlayer(chuckName))
		.pattern_(this).play;
 	}

	*>> { | symbol |
		^TaskPlayer(symbol).pattern_(this);
	}
}

// ================ INCOMPLETE: ================
/* // possibly:

'xo___' ><.pattternname \chuckname;

or:

pattern -> 'xo-filter' *> \taskname;

or 

{ func } ==> \chuckname => taskNameOrPattern;

{ func } ==> \chuckname =>.xofilter taskNameOrPattern;

Alternatively: 

{ func } ==> \chuckname *>.xofilter taskNameOrPattern;

*/


+ Symbol {

	*> { | taskName, xoPattern | // chuck -> task
		^Chuck(this).addToTask(taskName, XoPlayer(this, taskName, xoPattern))
		
	}

	/*
	!> { | taskName |
	
	}
	*/
	
}

XoPlayer {
	var <chuck, <task, <pattern, stream;

	
	
	*> { | symbol |
	
	}
}
