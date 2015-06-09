/*
Thu, Jun  4 2015, 12:29 EEST
Replaces ChuckProcess.

*/
ChuckSource {
	var <source;

	*new { | source |
		^this.newCopyArgs( (source ?? { ["x", 0] }).asStream)
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
		if (output respondsTo: \release) {
			if (output.isPlaying) {
				output.release(args[\fadeTime].next)
			}{  // TODO: replace this with onStart call for accuracy:
				SystemClock.sched (0.02, {
					if (output.isPlaying) {
						output.release(args[\fadeTime].next)
					};
					nil
				});
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
	
}

+ Object {
	asChuckSource { ^ChuckSource(this) }
}

+ Function {
	asChuckSource { ^ChuckSynthSource(this) }
}

+ String {
	asChuckSource { ^ChuckSynthSource(this) }
	play { | target, outbus, fadeTime, addAction, args |
		^Synth (this, args ++ [out: outbus, fadeTime: fadeTime], target, addAction)
	}
}

