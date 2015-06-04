+ Object {
	=> { | chuckName, adverb |
		^Chuck(chuckName).setArgs (adverb, this)
	}

	|> { | chuckName, pattern |
		^chuckName.sched (this.asStream, TempoClock (), pattern);
	}

	=|> { | symbol |  // add adverb to play in parameter!
		^Chuck (symbol) setSource: this;
	}

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

	|> { | master pattern |
		^Chuck (this).playSubPattern (Chuck (master), pattern)
	}

	asBeatPattern { ^Pseq(this.asString, inf) }
}

+ Chuck {
	|> { | master, pattern |
		^this.playSubPattern (Chuck (master), pattern)
	}

	playSubPattern { | master, pattern |
		var stream;
		stream =  (pattern ? 'x').asBeatPattern.asStream;
		^this.addNotifier (master, \play, { | value, notifier |
			if (value.isKindOf(Node) or: { value[1] == 0 } ) {
				this.addNotifier (master, \play, { | value, notifier |
					var matcher, initial;
					matcher = stream.next.asString;
					initial = matcher [0];
					case
					{ initial === $x } { this.play }
					{ initial === $o } { this.release; }
					{ initial === $_ } { /* no release */ }
					{ matcher includes: value [0] } { this.play }
					{ this.release }
				});
			}}
		)
	}
}

+ Function {
	=> { | symbol |  // add adverb to play in parameter!
		^Chuck (symbol).play (this);
	}

	|> { | master, sub |
		var playFunc;
		sub = Chuck (sub);
		
		^sub.addNotifier (Chuck (master), \play, { | notifier value |
			if (this.(value, notifier)) { sub.play }
		})
	}
}

+ String {
	=> { | symbol |  // add adverb to play in parameter!
		^Chuck (symbol).play (this);
	}

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