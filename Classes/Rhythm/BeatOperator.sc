/*
DRAFT!

Wed, May 27 2015, 08:45 EEST

set duration pattern of a Beat instance, and start playing: 

anObject || \symbol;

set duration pattern + beat pattern of a Beat instance, and start playing: 

anObject ||.beatpattern \symbol; 

Note: beatpattern can be a symbol, x denoting active beats, anything else non-active beats.

Set beat pattern of main beat of Beat \beat:
"xoxoxxx" || \beat
	// or:
'xoxoxxx' || \beat

Set beat pattern of subbeat pattern \bass of Beat \beat:
"xxxoo" ||.bass \beat 
	// or:
'xxxoo' ||.bass \beat 



*/

Beat {
	var <name, <durationPattern = 0.25;
	var <listeners; // dictionary of beat listeners
	var <durationStream, <task, <tempoClock;

	play { | beats, beatPattern |
		if (this.isPlaying) { ^this }; // don't restart if already playing ...
		// Following is a draft only!
		durationStream = durationPattern.asStream;
		task = Task ({
			var dur;
			while {
				(dur = durationStream.next).notNil
			}{
				listeners do: _.w.ime.uspgw.imbeat;
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
	
	| { | beat beatPattern = 'x' |
		// use adverb to introduce beat patterns like this: bpm |.xoxo \beat1
		postf ("This sets the basic duration pattern of a Beat: %, %\n", beat, beatPattern);

		Registry (Beat, beat, {
			Beat ()
		}).play (value, beatPattern)
	}
}

+ SimpleNumber {
	bpm { ^BPM(this / 60) }
}

+ String {
	| { | beat adverb = 'default' |
		// set the beat pattern of a Beat instance
		postf("beat test! adverb is: %\n", adverb);
	}
}

+ Symbol {
	// set the beat pattern of a Beat instance
	| { "symbol ok".postln; 
	}
}

// SHORTCUT FOR ".bpm !"
+ Object {
	// shortcut for .bpm ? !!!
	|| { "testing".postln; }
}