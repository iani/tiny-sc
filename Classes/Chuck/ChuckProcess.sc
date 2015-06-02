ChuckProcess {
	classvar >parentArgs; // Parent event holding default parameters for args
	var <chuck, <template, <args;
	var <argsTemplate; // TODO: store patterns of the param streams, for cloning

	*new { | chuck, template, args |
		^this.newCopyArgs (
			chuck,
			template,
			args ?? { ().parent_ (this.parentArgs) }
		).init;
	}

	init { }
	
	*parentArgs {
		parentArgs ?? {
			parentArgs = (
				outbus: 0,
				fadeTime: 0.02,
				addAction: \addToHead
			)
		};
		^parentArgs;
	}

	setArgs { | args |
		var theArgs, keysValues;
		theArgs = args [\args];
		theArgs ?? { args [\args] = theArgs = ().parent_ (args) };
		args keysValuesDo: { | key, value |
			value = value.asStream;
			keysValues = keysValues add: key;
			keysValues = keysValues add: value;
			theArgs [key] = value;
		};
		^keysValues;
	}
	
	setProcessParameter { | parameter, value |
		args [parameter] = value;
	}
	play {}

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

	outbus_ { | bus, slot = \out |
		[thisMethod.name, bus, slot].postln;
		if (slot === \out) { this.setProcessParameter(\outbus, bus) };
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

	init { synth = chuck.process.synth }
	
	play {
		this.synth = template.play(
			args [\target].next.asTarget,
			args [\outbus].next,
			args [\fadeTime].next,
			args [\addAction].next,
			(args [\args] ?? { () }).getPairs
		);
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
		result = args [\args] use: template.func;
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