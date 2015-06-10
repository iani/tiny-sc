/* 


Wed, Jun 10 2015, 20:35 EEST

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
INCOMPLETE - NOT TESTED! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

See: file:./ChainingTaskOperators.org
*/

+ Symbol {

	*> { | taskName, xoPattern | // chuck -> task
		^Chuck(this).addToTaskOrFilter(taskName, xoPattern, true);
	}

	**> { | taskName, xoPattern | // chuck -> task
		^Chuck(this).addToTaskOrFilter(taskName, xoPattern, false);
	}

	/*
	!> { | taskName |
	
	}
	*/
	
}

+ Chuck {

	*> { | taskName, xoPattern |
		this.addToTaskOrFilter(taskName, xoPattern, true);
	}

	**> { | taskName, xoPattern |
		this.addToTaskOrFilter(taskName, xoPattern, false);
	}

	addToTaskOrFilter { | taskName, xoPattern, play = false | // chuck -> task
		// TODO: integrate play switch!
		if (xoPattern.isNil) {
			^this.addToTask(taskName, play);
		}{
			^this.addToToxFilter(taskName, xoPattern, play);
		}
	}
	
	addToTask { | task, play = false |
		task = TaskPlayer(task);
		this.removePreviousTask;
		this.addNotifier(task, \beat, { this.play(task.dur) });
		this.addNotifier(task, \stop, { this.release });
		if (play) { task.play };
		^task;
	}

	addToFilter { | taskName, xoPattern, play = false |
		var task, filterTask;
		task = TaskPlayer(taskName);
		if (task isKindOf: Tox) {
			task.pattern = xoPattern;
			task = task.top;
			this.addToFilter(task);
		}{
			filterTask = Tox("_" ++ taskName, task, xoPattern);
			this.addToFilter(filterTask);
		};
		if (play) { task.play }
	}

	removePreviousTask {
		// this.removeMessage(\start); // not used for now
		this.removeMessage(\beat);
		this.removeMessage(\stop);
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

