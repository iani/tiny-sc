/*

================ DONE: ================

*>.adverb, **>.adverb: 

If task named by argument is Tox, then set new pattern.  Else create new Tox and set pattern

================ TODO: (Fri, Jun 12 2015, 12:11 EEST) ================

*>>.adverb, **>>.adverb: 

Always create new Tox and add it as filter to task player named by argument.

*/
+ Symbol {
	*> { | taskName, xoPattern | // chuck -> task
		^Chuck(this).addToTaskOrFilter(taskName, xoPattern, true);
	}

	**> { | taskName, xoPattern | // chuck -> task
		^Chuck(this).addToTaskOrFilter(taskName, xoPattern, false);
	}

	*>> { | taskName, xoPattern |
		^Chuck(this).addToxSubfilter(Task(taskName), xoPattern, true);
	}

	**>> { | taskName, xoPattern |
		^Chuck(this).addToxSubfilter(Task(taskName), xoPattern, false);
	}
	
	asTaskPlayer { ^TaskPlayer(this) }
	
	removeTask {
		^Registry.doIfFound(Chuck, this, { | c |
			c.removePreviousTask;
			c.release;
		})
	}
	stop {
		^Registry.doIfFound(TaskPlayer, this, { | t | t.stop });
	}

	start {
		^Registry.doIfFound(TaskPlayer, this, { | t | t.start });
	}
}

+ Chuck {

	*> { | taskName, xoPattern |
		this.addToTaskOrFilter(taskName, xoPattern, true);
	}

	**> { | taskName, xoPattern |
		this.addToTaskOrFilter(taskName, xoPattern, false);
	}

	*>> { | taskName, xoPattern |
		this.addToxSubfilter(Task(taskName), xoPattern, true);
	}

	**>> { | taskName, xoPattern |
		this.addToxSubfilter(Task(taskName), xoPattern, false);
	}


	addToTaskOrFilter { | taskName, xoPattern, play = false | // chuck -> task
		// TODO: integrate play switch!
		if (xoPattern.isNil) {
			^this.addToTask(taskName, play);
		}{
			^this.addToToxFilter(taskName, xoPattern.asString, play);
		}
	}
	
	addToTask { | task, play = false |
		task = task.asTaskPlayer(this);
		this.removePreviousTask;
		this.addNotifier(task, \beat, { this.play(task.dur) });
		this.addNotifier(task, \stop, { this.release });
		if (play) { task.play };
		^task;
	}

	removePreviousTask {
		// this.removeMessage(\start); // not used for now
		this.removeMessage(\beat);
		this.removeMessage(\stop);
	}

	addToToxFilter { | taskName, xoPattern, play = false |
		var task, filterTask;
		task = TaskPlayer(taskName);
		if (task isKindOf: Tox) {
			task.pattern = xoPattern;
			this.addToTask(task, play);
		}{
			this.addToxSubfilter(task, xoPattern, play);
		};
	}

	addToxSubfilter { | task, xoPattern, play = false |
		var filter;
		filter = Tox("_" ++ task.name);
		filter.addToTask(task).pattern_(xoPattern);
		this.addToTask(filter, play);
	}
}

+ Object {
	asTaskPlayer { | chuck, xoPattern |
		^TaskPlayer(chuck.name).pattern = this;
	}

	
	*> { | chuckName | // play immediately
		^Chuck(chuckName).addToTask(TaskPlayer(chuckName))
		.pattern_(this).play;
 	}

	// analogous to ++>
	**> { | symbol | // set pattern, but do not play
		^TaskPlayer(symbol).pattern_(this);
	}

	asToxPattern { ^this }
}

+ Ref {
	// for: { func } ++> \chuckname *>.xofilter `taskName *> filterName;
	receiveChuck { | chuck, filterPattern |
		// ^ChuckFilterReceiver(chuck, TaskPlayer(value), filterPattern);
	}
}

+ String {
	asToxPattern { | repeats = inf |  ^this.pseq(repeats) }
}