+ Object {
	=> { | chuckName, adverb | ^Chuck(chuckName).setArgs (adverb, this) }
	|> { | chuckName, pattern | ^chuckName.sched (this.asStream, TempoClock (), pattern); }
	==> { | symbol | ^Chuck (symbol).source = this; } // add adverb to play in parameter!
	asBeatPattern { ^this }
}

+ Symbol {
	=> { | reader, io = \in_out | ^Chuck (this).append (Chuck (reader), io); }
	!> { | master | ^Chuck (this).removeNotifier (Chuck (master), \play); }
	chuck { ^Chuck (this) }
	// can use dur =>.fadeTime \symbol instead, but this is shorter:
	//	fadeTime_ { | dur = 0.1 |  ^this.ft_ (dur); } // still shadowed by SynthTree. TODO: Scrap SynthTree
	ft_ { | dur = 0.1 | ^this.chuck.setArgs (\fadeTime, dur) }
	out_ { | bus = 0, slot = \out | ^this.chuck.setArgs(slot, bus); }
	free { ^Registry.doIfFound(Chuck, this, _.free); }
	release { | dur = 0.1 | ^Registry.doIfFound(Chuck, this, _.release (dur)); }
	play { ^Chuck (this).play; }
	sched { | dur = 1, clock | ^Chuck (this).sched (dur, clock ?? { TempoClock () }) }
	|> { | master pattern | ^Chuck (this).playSubPattern (Chuck (master), pattern) }
	asBeatPattern { ^Pseq(this.asString, inf) }
}

+ Chuck {
	|> { | master, pattern | ^this.playSubPattern (Chuck (master), pattern) }

	playSubPattern { | master, pattern |
		var stream;
		stream =  (pattern ? 'x').asBeatPattern.asStream;
		// Only follow one pattern.  Otherwise hanging synths ensue:
		this.removeMessage(\play); 
		^this.addNotifier (master, \play, { | key, argCount, notifier |
			if (argCount == 0) {
				this.addNotifier (master, \play, { | key, argCount, notifier |
					var matcher, initial;
					matcher = stream.next.asString;
					initial = matcher [0];
					case
					{ initial === $x } { this.play(key, argCount) }
					{ initial === $o } { this.release; }
					{ initial === $_ } { /* no release */ }
					{ matcher includes: key } { this.play(key, argCount) }
					{ this.release }
				});
			}}
		)
	}
}

+ Function {
	=> { | symbol | ^Chuck (symbol).source_(this).play; } // add adverb to play parameter!

	|> { | master, sub |
		var playFunc;
		sub = Chuck (sub);
		
		^sub.addNotifier (Chuck (master), \play, { | key, count, notifier |
			if (this.(key, count, notifier)) { sub.play }
		})
	}
}

+ String {
	=> { | symbol | ^Chuck (symbol).source_(this).play; } // add adverb to play parameter!

	|> { | master, sub |
		var pattern, stream, func;
		sub = Chuck (sub);
		pattern = Pseq (this.ascii, inf);
		func = { false };
		^sub.addNotifier (Chuck (master), \play, { | notifier, value |
			if (func.value) {
				sub.play;
			}{
				if (value [1] == 0) { // wait till cycle start
					stream = sub.asStream;
					func = { stream.next == 120 }; // play if char == #x
					if (func.value) { sub.play };
				}
			}
		})
	}
	
}

+ Method {
	notImplemented { postf ("% not implemented in %\n", name, ownerClass )}
}