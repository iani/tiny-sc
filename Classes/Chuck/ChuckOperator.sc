+ Object {
	asChuckProcess { | chuck, args |
		^this.chuckProcessClass.new(chuck, this, args);
	}

	=> { | symbol, adverb |
		^Chuck(symbol).setArgs (adverb, this)
	}
}

+ Function {
	=> { | symbol |
		^Chuck (symbol).play (this);
	}

	&> { | symbol |
		^Chuck (symbol).eval (this);
	}
}

+ Nil {
	chuckProcessClass { ^Cnil }
}

+ Function {
	chuckProcessClass { ^Csynth }
	
}

+ String {
	play { | target, outbus, fadeTime, addAction, args |
		^Synth (this, args ++ [out: outbus, fadeTime: fadeTime], target, addAction)
	}	
}
+ Symbol {
	chuck { ^Chuck (this) }
	// can use dur =>.fadeTime \symbol instead, but this is shorter:
	//	fadeTime_ { | dur = 0.1 |  ^this.ft_ (dur); } // still shadowed by SynthTree. TODO: Scrap SynthTree
	ft_ { | dur = 0.1 | ^this.chuck.setArgs (\fadeTime, dur) }
	out_ { | bus = 0, slot = \out |
		^this.chuck.setArgs(slot, bus);
	}
	dur_ { | dur = 1 | ^this.chuck.dur = dur }
	clock_ { | clock | ^this.chuck.clock = clock }
	free { ^Registry.doIfFound(Chuck, this, _.free); }
	release { | dur = 0.1 |
		^Registry.doIfFound(Chuck, this, _.release (dur));
	}
	play { | dur |
		var chuck;
		chuck = Chuck (this);
		dur !? { chuck.dur = dur };
		^chuck.play;
	}

	// Bus stuff
	@> { | chuckName, io = \in_out |
		^Chuck (this).append (Chuck (chuckName), io);
	}	
}

+ Method {
	notImplemented { postf ("% not implemented in %\n", name, ownerClass )}
}