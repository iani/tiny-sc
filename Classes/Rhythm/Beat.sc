/* Wed, May 27 2015, 08:45 EEST */
Beat {
	classvar <listeners;

	var <name, <parent, path;

	*initClass { listeners = IdentityDictionary () }

	*new { | name, parent |
		var path;
		path = this.makePath(name, parent);
		[thisMethod.name, path].postln;
		
		^Registry(Beat, path.asSymbol, {
			"was not found, making new".postln;
			this.newCopyArgs(name.asSymbol, parent, path)
		});
	}

	*getFromPath { | path |
		if (path.asString includes: $_) {
			^BeatPattern(path)
		}{
			^BeatPlayer(path)
		}
	}

	*makePath { | name, parent |
		if (parent.isNil) {
			^name.asSymbol;
		}{
			^(parent.makePath ++ _ ++ name).asSymbol;
		}
	}

	add { | object, action |
		this.class.add(path, object, action);
	}
	
	*add { | path, object, action |
		var myPaths;
		myPaths = listeners [object] ?? { Set () };
		listeners [object] = myPaths add: path;
		object.addNotifier(Beat, path, action);
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
		myPaths do: { | p |
			object.removeNotifier (Beat, p);
		};
		listeners [object] = nil;
	}
}

BeatPattern : Beat {
	var pattern, stream;

	enable {
		Beat.add(parent.path, this, { this.play });
	}

	play {
		if (stream.next) { Beat.changed(path) }
	}
	
	disable {
		Beat.remove(parent.path, this);
	}
	
	pattern_ { | argPattern |
		this.setPattern(argPattern.asBeatPattern);
	}

	setPattern { | argPattern |
		postf("setPattern: %\n", argPattern).postln;
		pattern = argPattern;
		stream = pattern.asStream;
		postf("setPattern: %\n !!!! after !!!", argPattern).postln;
   }
}

BeatPlayer : BeatPattern {
	var task, clock, <dur;
	
	pattern_ { | argPattern | this.setPattern(argPattern) }

	start { | defaultPattern |
		if (task.isPlaying) { ^this };
		if (task.isNil or: { task.streamHasEnded }) {
			this.reset;
		};
		task.start;
	}

	reset {
		this.makeStream;
		this.makeTask;
	}
	makeStream { this.pattern = pattern; } // resets stream
	stop { task.stop; }

	restart {
		this.reset;
		this.start;
	}

	makeTask { 
		task = Task ({
			while {
				(dur = stream.next).notNil
			}{
				Beat.changed (path);
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