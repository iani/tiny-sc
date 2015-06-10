
TaskFilter { // subclasses implement different filter methods
	// for now, only one task  or filter can be parent;
	var parent;
	
	addToTask { | task |
		this.removeFromTask;
		parent = task; 
		this.addNotifier(task, \start, { this.start });
		this.addNotifier(task, \beat, { this.beat});
		this.addNotifier(task, \stop, { this.stop });
		^task;
	}
	
	removeFromTask { 
		// this.release; // called by other methods higher-up in the call chain?
		this.removeMessage(\start);
		this.removeMessage(\beat);
		this.removeMessage(\stop);
		parent = nil;
	}
	
	start {}
	beat { thisMethod.subclassResponsibility }
	stop {}
	dur { ^parent.dur }
	count { ^parent.count }
	val { ^this.topval }
	topval { ^parent.topval } // top value (TaskPlayer val ...)
}

Tox : TaskFilter {
	var pattern, stream, <val;

	pattern_ { | argPattern |
		pattern = argPattern;
		stream = pattern.asStream;
	}
	
	beat {
		val = stream.next;
		switch (val,
			$x, { this.changed(\beat) },
			$o, { this.changed(\stop) }			
		)
	}	
}