/*
DRAFT!

Wed, May 27 2015, 08:45 EEST
*/

Beat {
	var <name, <beatPattern = "x", <durationPattern = 0.25;
	var <listeners; // dictionary of beat listeners
	var <durationStream, <task, <tempoClock;

	play {
		durationStream = durationPattern.asStream;
		task = Task ({
			var dur;
			while {
				(dur = durationStream.next).notNil
			}{
				listeners do: _.beat;
				dur.wait;
			}
		}).play;
	}
}

BeatListener {
	var <beatPattern, <listeners;
	var <beatStream;

	beat {
		// see BeatStream / Pbeatpat
		if (beatStream.next) { listeners do: _.play; }
	}
}

BeatStream {
	// a stream that translates x and o to beat / no beat.
	// may rewrite as new filterpattern class: Pbeatpat, 
}


BPM : Ref {
	
	| { | beat adverb = 'x' |
		// use adverb to introduce beat patterns like this: bpm |.xoxo \beat1
		postf ("This sets the basic duration pattern of a Beat: %, %\n", beat, adverb);
	}
	
}

+ SimpleNumber {
	bpm { ^BPM(this / 60) }
}

+ String {
	| { | beat adverb = 'default' |
		postf("beat test! adverb is: %\n", adverb);
	}
}

+ Symbol {
	| { "symbol ok".postln; 
	}
}