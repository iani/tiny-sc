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
		valStream = valPattern.asStream;
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
				count = counter.next;
				val = valStream.next;
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

XoPlayer {
	var <chuck, <task, <pattern, stream;
	
	*> { | symbol |
	
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
	*> { | chuckName | // play immediately
		^Chuck(chuckName).addToTask(TaskPlayer(chuckName))
		.pattern_(this).play;
 	}

	// analogous to ==>
	**> { | symbol | // set pattern, but do not play
		^TaskPlayer(symbol).pattern_(this);
	}

	// TODO: Implement
	*>> { // play with synonymous filter of this task, instead of task
		thisMethod.notImplemented;
	}
}

+ Ref {
	// for: { func } ==> \chuckname *>.xofilter `taskName *> filterName;
	receiveChuck { | chuck, filterPattern |
		// ^ChuckFilterReceiver(chuck, TaskPlayer(value), filterPattern);
	}
}

/*
ChuckFilterReceiver {
	var chuck, task, filterPattern;

	*> { | filterName | 
	filterPattern.asTaskFilter(filterName).connectChuckTask(chuck, task);
	}

}
*/

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

To specify taskfilter by name: 

{ func } ==> \chuckname *>.xofilter `taskName *> filterName;

Task-filters: 
Registry(TaskFilter, \taskname, \filtername, { ... });
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
