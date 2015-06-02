ChuckProcess {
	classvar >parentArgs; // Parent event holding default parameters for args
	var <chuck, <template, <args;
	var <dur, <>clock;
	var <argsTemplate; // TODO: store patterns of the param streams, for cloning

	*new { | chuck, template, args |
		^this.newCopyArgs (
			chuck,
			template,
			args ?? { ().parent_ (this.parentArgs) }
		).init;
	}

	init {
		//		synth = chuck.process.synth;
		var process;
		process = chuck.process;
		if (process.notNil) {
			clock = chuck.process.clock ?? { TempoClock.default };
			dur = chuck.process.dur;
		}{
			clock = TempoClock.default;
		};
	}
	
	*parentArgs {
		parentArgs ?? {
			parentArgs = (
				out: 0,
				fadeTime: 0.02,
				addAction: \addToHead
			)
		};
		^parentArgs;
	}

	setArgs { | theArgs |
		var keysValues;
		theArgs keysValuesDo: { | key, value |
			value = value.asStream;
			keysValues = keysValues.add (key).add (value);
			args [key] = value;
		};
		^keysValues;
	}
	
	play { | argDur |
		argDur !? { this.dur = argDur };
		this.sched;
	}

	dur_ { | argDur | dur = argDur.asStream }

	sched {
		clock.sched (dur.next, {
			this.release;
			this.play;
		})
	}

	synth { ^nil }
	
	setWriterAudioTarget { | buslink, slot = \out |
		// write your output to buslink
		this.moveToHead(buslink);
		this.target = buslink;
		//		this.outbus_()
	}
	setReaderAudioTarget { | buslink, slot = \in |
		// read your input from buslink

	}

	out_ { | bus, slot = \out |
		this.setArgs ([slot, bus]);
	}

	fadeTime_ { | dur = 0.1 |
		this.setProcessParameter(\fadeTime, dur);
	}
	
}


Cnil : ChuckProcess {
	// Just a consistent name for the empty ChuckProcess
}

Csynth : ChuckProcess {
	var <synth;

	
	init {
		synth = chuck.process.synth;
		super.init;
	}

	play { | argDur |
		this.synth = template.play(
			args [\target].next.asTarget,
			args [\out].next,
			args [\fadeTime].next,
			args [\addAction].next,
			args.getPairs
		);
		super.play (argDur);
	}
	
	synth_ { | argSynth |
		synth = argSynth;
		synth.onEnd (this, { this.changed (\synthStopped)});
	}
	
	stop { this.release }
	release { | dur |
		synth !? {
			if (synth.isPlaying) {
				synth.release(dur ?? { args[\fadeTime].next })
			}{
				synth.onStart (this, { 
					synth.release(dur ?? { args[\fadeTime].next })
				})
			}
		}
	}

	free {
		synth !? {
			if (synth.isPlaying) {
				synth.free;
			}{
				synth.onStart (this, { synth.free })
			}
		}
	}

	setArgs { | args |
		synth.set (*super.setArgs (args));
	}
	
	setProcessParameter { | parameter, value |
		super.setProcessParameter (parameter, value);
		this.perform ((parameter ++ "_").asSymbol, value);
	}

	target_ { | atarget |
		
	}

	/*
	outbus_ { | aoutbus |
		synth.set (\outbus, aoutbus);
	}
	*/
	fadeTime_ { | afadeTime |
		synth.set (\fadeTime, afadeTime);
	}

	addAction_ { | aaddAction |
		
	}

	args_ { | aargs |
		// TODO: Review this!
		synth.set (*aargs)
	}
}

Cfunc : Csynth {
	play {
		var result;
		result = args use: template.func;
		if (result isKindOf: Node) {
			this.synth = result;
		}
	}
}

CfuncTemplate {
	var <func;
	*new { | func | ^this.newCopyArgs(func) }

	chuckProcessClass { ^Cfunc }
}