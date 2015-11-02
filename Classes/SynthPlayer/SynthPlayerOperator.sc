+ Object {
	+> { | synthPlayerName, adverb | ^SynthPlayer(synthPlayerName).setArgs (adverb, this) }
}

+ Event {
	+> { | synthPlayerName | ^SynthPlayer(synthPlayerName).setArgs(*this.getPairs) }
}

+ SequenceableCollection {
	+> { | synthPlayerName | ^SynthPlayer(synthPlayerName).setArgs(*this) }
}

+ SynthPlayer {
	+> { | reader, io = \in_out | ^this.append (SynthPlayer (reader), io); }
	receiveFunc { | func, param |
		case
		{ param.isNil } { this.source_(func) }
		{ param isKindOf: SimpleNumber } {
			this.args[\dur] = param;
			this.source_(func);
		}
		{ this.setArgs(param, func) };
	}
}

+ Symbol {
	+> { | reader, io = \in_out |
		^SynthPlayer (this).append (SynthPlayer (reader),
			*io.asString.split($_).collect(_.asSymbol));
	}
	!> { | reader, io = \in_out |
		^SynthPlayer (this).removeReader (SynthPlayer(reader),
			*io.asString.split($_).collect(_.asSymbol));
	}
	synthPlayer { ^SynthPlayer (this) }
	// can use dur +>.fadeTime \symbol instead, but this is shorter:
	fadeTime_ { | dur = 0.1 |  ^this.ft_ (dur); }
	ft_ { | dur = 0.1 | ^this.synthPlayer.setArgs (\fadeTime, dur) }
	out_ { | bus = 0, slot = \out | ^this.synthPlayer.setArgs(slot, bus); }
	free { ^Registry.doIfFound(SynthPlayer, this, _.free); }
	release { | dur = 0.1 | ^Registry.doIfFound(SynthPlayer, this, _.release (dur)); }
	play { ^SynthPlayer (this).play; }
	sched { | dur = 1, clock | ^SynthPlayer (this).sched (dur, clock ?? { TempoClock () }) }
	//	|> { | master pattern | ^SynthPlayer (this).playSubPattern (SynthPlayer (master), pattern) }
	asBeatPattern { ^Pseq(this.asString, inf) }
	target { ^SynthPlayer(this).target }
	toRoot { ^SynthPlayer(this).toRoot }
}

+ Function {
	+> { | symbol, adverb |  ^SynthPlayer(symbol).receiveFunc(this, adverb); }
	++> { | symbol, adverb | ^SynthPlayer(symbol).receiveFunc(this, adverb).play }
}

+ Ref { // `{ } quotes function so that it evals rather than {}.plays
	+> { | symbol |
		^SynthPlayer(symbol).source_(SynthPlayerSource(value)) }
	++> { | symbol |
		^SynthPlayer(symbol).source_(SynthPlayerSource(value)).play }
}

+ String {
	+> { | symbol | ^SynthPlayer (symbol).source_(this); } // add adverb to play parameter!
	++> { | symbol | ^SynthPlayer (symbol).source_(this).play; } // add adverb to play parameter!
}

+ Method {
	notImplemented { postf ("% not implemented in %\n", name, ownerClass )}
}
