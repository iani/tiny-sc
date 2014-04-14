/*
Play a stream from a pattern with a Task, scheduling the calls of "next" to the value stream from a stream of durations.  Use custom action function to dispatch the values. 

IZ Mon, Mar 10 2014, 01:53 EET

UNDER CONSTRUCTION

*/

StreamPattern {
	var <pattern;
	var <durations;
	var <clock;
	var <stream;
	var <task;
	var <duration;   // duration of current interval till next step

	*new { | synthTree, name, initialValue = 0 |
		^this.newCopyArgs(synthTree, name, initialValue, NullSpec);
	}


	start {

	}

	stop {

	}

	reset {

	}

	isPlaying {

	}

}