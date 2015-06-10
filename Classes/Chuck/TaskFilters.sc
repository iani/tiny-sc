
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
	
	start { this.changed(\start) }
	beat { thisMethod.subclassResponsibility }
	stop { this.changed(\stop) }
	dur { ^parent.dur }
	count { ^parent.count }
	val { ^this.topval }
	topval { ^parent.topval } // top value (TaskPlayer val ...)
}

TPatFilter : TaskFilter {
	var pattern, stream, <val;

	pattern_ { | argPattern |
		pattern = argPattern;
		stream = pattern.asStream;
	}

	beat { // only get value.  Subclasses choose whether to beat
		val = stream.next;
	}
}

Tox : TPatFilter {
	beat {
		super.beat;
		switch (val,
			$x, { this.changed(\beat) },
			$o, { this.changed(\stop) }			
		)
	}	
}

Tbartox : Tox {
	var startBeat = 0;
	var waiting = true;
	beat {
		if (waiting and: { startBeat != parent.val }) {
			// skip till first beat
		}{
			waiting = false;
			super.beat;
		}
	}	
}

