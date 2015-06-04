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

+ Symbol {
	=> { | reader, io = \in_out | ^Chuck (this).append (Chuck (reader), io); }

	|> { | master, pattern |
		var sub, playFunc;
		sub = Chuck (this);
		if (pattern.size == 0) {
			playFunc = { sub.play }
		}{
			pattern = pattern.asString;
			playFunc = { | notifier, value |
				if (pattern includes: value [0]) { sub.play }
			}
		};
		^sub.addNotifier (Chuck (master), \play, playFunc)
	}

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
}

+ Method {
	notImplemented { postf ("% not implemented in %\n", name, ownerClass )}
}