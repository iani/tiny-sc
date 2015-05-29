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

	var <name, <durationPattern = 0.25;
	var <durationStream, <task, <tempoClock;

*/

Beat {
	classvar <listeners;

	var <name, <parent, path;

	*initClass { listeners = IdentityDictionary () }

	*new { | name, parent |
		var path;
		path = this.makePath(name, parent);
		^Registry(path, {
			this.newCopyArgs(name.asSymbol, parent, path)
		});
	}

	*makePath { | name, parent |
		if (parent.isNil) {
			^name.asSymbol;
		}{
			^(parent.makePath ++ _ ++ name).asSymbol;
		}
	}

	enable {
		this.addNotifier(Beat, path, { this.play; });
	}

	disable {
		this.removeNotifier(Beat, path);
	}

	*add { | path, object |
		var myPaths;
		myPaths = listeners [object] ?? { Set () };
		listeners [object] = myPaths add: path;
	}

	*remove { | path, object |
		var myPaths;
		myPaths = listeners [object];
		myPaths !? {
			object.removeNotifier (Beat, path);
			myPaths remove: path;
			if (myPaths.size == 0) {
				listeners [object] = nil;
			}{
				listeners [object] = myPaths;
			}
		}
	}

	*add1 { | path, object |
		this.removeAll (object);
		this.add (path, object);
	}

	*removeAll { | object |
		var myPaths;
		myPaths = listeners [object];
		myPaths.copy do: { | p |
			object.removeNotifier (Beat, p);
		};
		listeners [object] = nil;
	}
}

BeatPattern : Beat {
	var pattern, stream;

   pattern_ { | argPattern |
	   pattern = argPattern;
	   stream = pattern.asStream;
   }

	beat {
		
   }
}

BeatPlayer : BeatPattern {
	var task, clock, <dur;
	
	play { 
		this.pattern = pattern; // resets stream
		task = Task ({
			while {
				(dur = stream.next).notNil
			}{
				Beat.changed (path, this);
				dur.wait;
			};
		}).play;
	}
}


BeatFilter {
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