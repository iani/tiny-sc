+ Object {
	+> { | chuckName, adverb | ^Chuck(chuckName).setArgs (adverb, this) }
}

+ Event {
	+> { | chuckName | ^Chuck(chuckName).setArgs(*this.getPairs) }
}

+ SequenceableCollection {
	+> { | chuckName | ^Chuck(chuckName).setArgs(*this) }
}

+ Chuck {
	+> { | reader, io = \in_out | ^this.append (Chuck (reader), io); }
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
		^Chuck (this).append (Chuck (reader),
			*io.asString.split($_).collect(_.asSymbol));
	}
	!> { | reader, io = \in_out |
		^Chuck (this).removeReader (Chuck(reader),
			*io.asString.split($_).collect(_.asSymbol));
	}
	chuck { ^Chuck (this) }
	// can use dur +>.fadeTime \symbol instead, but this is shorter:
	fadeTime_ { | dur = 0.1 |  ^this.ft_ (dur); }
	ft_ { | dur = 0.1 | ^this.chuck.setArgs (\fadeTime, dur) }
	out_ { | bus = 0, slot = \out | ^this.chuck.setArgs(slot, bus); }
	free { ^Registry.doIfFound(Chuck, this, _.free); }
	release { | dur = 0.1 | ^Registry.doIfFound(Chuck, this, _.release (dur)); }
	play { ^Chuck (this).play; }
	sched { | dur = 1, clock | ^Chuck (this).sched (dur, clock ?? { TempoClock () }) }
	//	|> { | master pattern | ^Chuck (this).playSubPattern (Chuck (master), pattern) }
	asBeatPattern { ^Pseq(this.asString, inf) }
	target { ^Chuck(this).target }
	toRoot { ^Chuck(this).toRoot }
}

+ Function {
	+> { | symbol, adverb |  ^Chuck(symbol).receiveFunc(this, adverb); }
	++> { | symbol, adverb | ^Chuck(symbol).receiveFunc(this, adverb).play }
}

+ Ref { // `{ } quotes function so that it evals rather than {}.plays
	+> { | symbol |
		^Chuck(symbol).source_(ChuckSource(value)) }
	++> { | symbol |
		^Chuck(symbol).source_(ChuckSource(value)).play }
}

+ String {
	+> { | symbol | ^Chuck (symbol).source_(this); } // add adverb to play parameter!
	++> { | symbol | ^Chuck (symbol).source_(this).play; } // add adverb to play parameter!
}

+ Method {
	notImplemented { postf ("% not implemented in %\n", name, ownerClass )}
}
