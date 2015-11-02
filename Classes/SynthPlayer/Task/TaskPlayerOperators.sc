/*

================ DONE: ================

*>.adverb, **>.adverb: 

If task named by argument is Tox, then set new pattern.  Else create new Tox and set pattern

================ TODO: (Fri, Jun 12 2015, 12:11 EEST) ================

*>>.adverb, **>>.adverb: 

Always create new Tox and add it as filter to task player named by argument.

*/
+ Symbol {
	*> { | taskName, xoPattern | // synthPlayer -> task
		^SynthPlayer(this).addToTaskOrFilter(taskName, xoPattern, true);
	}

	**> { | taskName, xoPattern | // synthPlayer -> task
		^SynthPlayer(this).addToTaskOrFilter(taskName, xoPattern, false);
	}

	*>> { | taskName, xoPattern |
		^SynthPlayer(this).addToxSubfilter(TaskPlayer(taskName), xoPattern, true);
	}

	**>> { | taskName, xoPattern |
		^SynthPlayer(this).addToxSubfilter(TaskPlayer(taskName), xoPattern, false);
	}
	
	asTaskPlayer { ^TaskPlayer(this) }
	
	removeTask {
		^Registry.doIfFound(SynthPlayer, this, { | c |
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

+ SynthPlayer {

	*> { | taskName, xoPattern |
		this.addToTaskOrFilter(taskName, xoPattern, true);
	}

	**> { | taskName, xoPattern |
		this.addToTaskOrFilter(taskName, xoPattern, false);
	}

	*>> { | taskName, xoPattern |
		this.addToxSubfilter(TaskPlayer(taskName), xoPattern.asString, true);
	}

	**>> { | taskName, xoPattern |
		this.addToxSubfilter(TaskPlayer(taskName), xoPattern.asString, false);
	}


	addToTaskOrFilter { | taskName, xoPattern, start = false | // synthPlayer -> task
		// TODO: integrate start switch!
		if (xoPattern.isNil) {
			^this.addToTask(taskName, start);
		}{
			^this.addToToxFilter(taskName, xoPattern.asString, start);
		}
	}
	
	addToTask { | task, start = false |
		task = task.asTaskPlayer(this);
		this.removePreviousTask;
		this.addNotifier(task, \beat, { this.play(task) });
		this.addNotifier(task, \stop, { this.release }); // make sure if starting very fast
		if (start) { task.start };
		^task;
	}

	removePreviousTask {
		// this.removeMessage(\start); // not used for now
		this.removeMessage(\beat);
		this.removeMessage(\stop);
	}

	addToToxFilter { | taskName, xoPattern, start = false |
		var task, filterTask;
		task = TaskPlayer(taskName);
		if (task isKindOf: Tox) {
			task.pattern = xoPattern;
			this.addToTask(task, start);
		}{
			this.addToxSubfilter(task, xoPattern, start);
		};
	}

	addToxSubfilter { | task, xoPattern, start = false |
		var filter;
		filter = Tox("_" ++ task.name);
		filter.addToTask(task).pattern_(xoPattern);
		this.addToTask(filter, start);
	}
}

+ Object {
	asTaskPlayer { | synthPlayer, xoPattern |
		^TaskPlayer(synthPlayer.name).pattern = this;
	}

	
	*> { | synthPlayerName | // start immediately
		^SynthPlayer(synthPlayerName).addToTask(TaskPlayer(synthPlayerName))
		.pattern_(this).start;
 	}

	// analogous to ++>
	**> { | symbol | // set pattern, but do not start
		^TaskPlayer(symbol).pattern_(this);
	}

	asToxPattern { ^this }
}

+ Ref {
	// for: { func } ++> \synthPlayername *>.xofilter `taskName *> filterName;
	receiveSynthPlayer { | synthPlayer, filterPattern |
		// ^SynthPlayerFilterReceiver(synthPlayer, TaskPlayer(value), filterPattern);
	}
}

+ String {
	asToxPattern { | repeats = inf |  ^this.pseq(repeats) }
}