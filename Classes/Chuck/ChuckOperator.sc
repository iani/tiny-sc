+ Object {
	asChuckProcess { | chuck, args |
		^this.chuckProcessClass.new(chuck, this, args);
	}

	=> { | symbol, adverb |
		^Registry.doIfFound (Chuck, symbol, { | chuck |
			chuck.setProcessParameter (adverb, this)
		})
	}

	*> { | symbol, adverb |
		^Registry.doIfFound (Chuck, symbol, { | chuck |
			chuck.setArgs (adverb, this)
		})
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
	fadeTime_ { | dur = 0.1 |  ^this.ft_ (dur); }
	ft_ { | dur = 0.1 | ^this.chuck.setProcessParameter (\fadeTime, dur) }
	outbus_ { | bus, slot = \out |
		[thisMethod.name, this, bus, slot].postln;
		^this.chuck.outbus_(bus, slot);
		// ^this.chuck.process.outbus_(bus, slot);
	}
	free { ^Registry.doIfFound(Chuck, this, _.free); }
	release { | dur = 0.1 |
		^Registry.doIfFound(Chuck, this, _.release (dur));
	}
	play { | func |
		^Registry.doIfFound(Chuck, this, _.play (func));
	}

	// Bus stuff
	@> { | chuckName, io = \in_out |
		^Chuck (this).append (Chuck (chuckName), io);
	}	
}

+ Method {
	notImplemented { postf ("% not implemented in %\n", name, ownerClass )}
}