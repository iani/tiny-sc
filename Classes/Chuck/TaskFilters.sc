/* 
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	TODO: 
	
	Rewrite start/stop to disconnect / reconnect to parent when starting/stopping
	
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*/

TaskFilter { // subclasses implement different filter methods
	// for now, only one task or filter can be parent;
	var <name, <parent;

	*new { | name |
		name = name.asSymbol;
		^Registry(TaskPlayer, name, { this.newCopyArgs(name) });
	}
	
	addToTask { | task |
		this.removeFromTask;
		parent = task;
		this.addNotifier(task, \start, { this.start });
		this.addNotifier(task, \beat, { this.beat });
		this.addNotifier(task, \stop, { this.stop });
		//	^task;
	}

	// NEW:
	connectToParent {
		this.addNotifier(parent, \start, { this.start });
		this.addNotifier(parent, \beat, { this.beat });
		this.addNotifier(parent, \stop, { this.stop });		
	}
	
	removeFromTask { 
		// this.release; // called by other methods higher-up in the call chain?
		this.removeMessage(\start);
		this.removeMessage(\beat);
		this.removeMessage(\stop);
		parent = nil;
	}

	// NEW:
	disconnectFromParent {
		this.removeMessage(\start);
		this.removeMessage(\beat);
		this.removeMessage(\stop);
	}

	/* 
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		Rewrite these to disconnect / reconnect to parent when starting/stopping

		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	*/
	start {
		this.connectToParent;
		parent.start;
	}
	// start { this.changed(\start) }
	beat { thisMethod.subclassResponsibility }
	stop { this.changed(\stop) }
	dur { ^parent.dur }
	count { ^parent.count }
	val { ^this.topval }
	topval { ^parent.topval } // top value (TaskPlayer val ...)
	top { ^parent.top }
	asTaskPlayer { ^this }
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

	pattern_ { | patternTemplate, repeats = inf |
		super.pattern_(patternTemplate.asToxPattern(repeats) );
	}
	
	beat {
		super.beat;
		if (val.isNil) {
			this.changed(\stop)
		}{
			switch (val,
				$x, { this.changed(\beat); },
				$o, { this.changed(\stop); }
			)
		}
	}	
}

Tbartox : Tox {
	var <>startBeat = 0;
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

