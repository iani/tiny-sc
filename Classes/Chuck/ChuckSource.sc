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
		this.moveToNullGroupIfNeeded(def);
		def.doSend(chuck.args[\target].server);
	}

	moveToNullGroupIfNeeded { | def |
		"Effect Chuck's reading from 0 bus may bleed through.".postln;
		thisMethod.notImplemented;
		/*  TODO:
            It is not possible to find the input ControlNames of UGens inside the SynthDef 
            that are of Class In and rate \audio, because there is no indication
            of the names of the controls inside the UGens content in the def. 
			Therefore rely on convention: if args contains keys whose name starts with
			"in", then assume these are *AUDIO* inputs.  If any values of these keys
			are not BusLinks, then assume that the Chuck has not been linked. 
			In that case, set its args[\target] to GroupLink.nullGroup.
		*/
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

