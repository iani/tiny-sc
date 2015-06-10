+ Chuck {
	addToTask { | task |
		this.removePreviousTask;
		this.addNotifier(task, \beat, { this.play(task.dur) });
		this.addNotifier(task, \stop, { this.release });
		^task;
	}

	removePreviousTask {
		// this.removeMessage(\start); // not used for now
		this.removeMessage(\beat);
		this.removeMessage(\stop);
	}
	
	removeFromTask { | task |
		// this.release; // called by other methods higher-up in the call chain?
		this.removeNotifier(task, \beat);
		this.removeNotifier(task, \stop);
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