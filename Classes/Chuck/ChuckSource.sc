/*
Thu, Jun  4 2015, 12:29 EEST
Replaces ChuckProcess.

*/
ChuckSource {
	var <source, <chuck;

	*new { | source, chuck |
		^this.newCopyArgs( (source ?? { ["x", 0] }).asStream, chuck)
	}

	play { | output, args |
		/*
			if (portamento.next) {
			     this.setArgs(output, args);
			}{
			// insert stop previous output here
                 this.prPlay(args);
			}
		*/
		if (output isKindOf: Node) {
			if (output.isPlaying) {
				// [thisMethod.name, "releasing playing output", output].postln;
				output.release(args[\fadeTime].next)
			}{  
				// [thisMethod.name, "sending onStart to non0 playing output", output].postln;
				output.onStart (this, { | notification |
 					if (notification.listener.isPlaying) {
						notification.listener.release(args[\fadeTime].next)
					}
				})
				/*
				SystemClock.sched (0.02, {
					if (output.isPlaying) {
						output.release(args[\fadeTime].next)
					};
					nil
					});
				*/
			}
		};
		^this.prPlay(args)
	}

	prPlay { | args |
		^args use: source;
	}

	asChuckSource { ^this }
}

ChuckSynthSource : ChuckSource {
	prPlay { | args |
		^source.play(
			args [\target].next.asTarget,
			args [\out].next,
			args [\fadeTime].next,
			args [\addAction].next,
			args.getPairs
		).register;
	}

	/*
		prPlay { | args |
		^Synth (this,
           args.getPairs ++ [out: outbus, fadeTime: fadeTime], target, addAction)
		}
	*/
}

ChuckFuncSynthSource : ChuckSynthSource {
	var <defName;

	*new { | source, chuck |
		^super.new(source, chuck).makeSynthDef;
	}

	makeSynthDef {
		var def;
		def = source.asSynthDef(
			fadeTime: chuck.args[\fadeTime],
			name: this.makeDefName
		);
		def.doSend(chuck.args[\target].server);
	}
	
	prPlay { | args |
		// play func only the first time.
		// thereafter, create synth from defName
		if (defName.isNil) {
			^source.cplay(
				args [\target].next.asTarget,
				args [\out].next,
				args [\fadeTime].next,
				args [\addAction].next,
				args.getPairs,
				this.makeDefName;
			).register;
		}{
			^defName.play(
				args [\target].next.asTarget,
				args [\out].next,
				args [\fadeTime].next,
				args [\addAction].next,
				args.getPairs
			).register;
		}
	}

	makeDefName {
		var theName;
		theName = format("<%>", chuck.name);
		{ defName = theName }.defer(0.1);
		^theName;
	}
}


+ Object {
	asChuckSource { | chuck | ^ChuckSource(this, chuck) }
}

+ Function {
	asChuckSource { | chuck | ^ChuckFuncSynthSource(this, chuck) }
}

+ String {
	asChuckSource { | chuck |  ^ChuckSynthSource(this, chuck) }
	play { | target, outbus, fadeTime, addAction, args |
		^Synth (this, args ++ [out: outbus, fadeTime: fadeTime], target, addAction)
	}
}

