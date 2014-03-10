/*
Hold a (synth) argument name, a pattern, the stream produced from it, the current value of the stream, a durations pattern, a durations stream, and the current Task playing the stream.

To be used as an element in an event in var args of SynthTree, for producing the values of an argument for creating each subsequent synth of the SynthTree instance, but also for setting values of controls indepenedently, if played with its own duration pattern. 

IZ Mon, Mar 10 2014, 01:53 EET

UNDER CONSTRUCTION

*/

StreamPattern {
	var <synthTree;
	var <name;
	var <valuePattern;
	var <durationPattern;
	var <valueStream;
	var <durationStream;
	var <task;
	var currentValue;
	var <>startAction; /* message to send to self when sent the message start 
		by the synthTree.  If \start, this will start playing the own pattern, 
		provided there are durations to play (NEEDS REFINEMENT) */


	*new { | synthTree, name |
		
	}

	next {
		if (task.isPlaying) {
			^currentValue ?? { currentValue = valueStream.next };
		}{
			^currentValue = valueStream.next;
		}
	}

	start {
	}

	stop {

	}

	reset {

	}

}