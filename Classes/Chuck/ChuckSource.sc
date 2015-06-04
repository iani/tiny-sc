/*
Thu, Jun  4 2015, 12:29 EEST
Replaces ChuckProcess.

*/
ChuckSource {
	var <source;

	*new { | source | ^this.newCopyArgs(source) }

	play { | output, args, chuck |
		if (output respondsTo: \release) {
			if (output.isPlaying) {
				output.release(args[\fadeTime].next)
			}{
				SystemClock.sched (0.02, {
					if (output.isPlaying) {
						output.release(args[\fadeTime].next)
					};
					nil
				});
			}
		};
		^this.prPlay(args, chuck)
	}

	prPlay { | args, chuck |
		^args use: source;
	}
	
}

ChuckSynthSource : ChuckSource {
	prPlay { | args, chuck |
		^source.play(
			args [\target].next.asTarget,
			args [\out].next,
			args [\fadeTime].next,
			args [\addAction].next,
			args.getPairs
		).register;
	}
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

