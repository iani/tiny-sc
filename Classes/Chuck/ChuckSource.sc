/*
Thu, Jun  4 2015, 12:29 EEST
*/
ChuckSource {
	var <source, <chuck;
	var <hasNoDurControl = false, <previousHasNoDurControl = false;
	var hasAudioInputs = false;

	*new { | source, chuck |
		^this.newCopyArgs( (source ?? { ["x", 0] }).asStream, chuck).init
	}

	init {
		if (chuck.notNil and: { chuck.source.notNil}) { 
			previousHasNoDurControl = chuck.source.hasNoDurControl;
		};
		this.examineSource;
	}

	examineSource {}

	play { | output, args |
		/*
			if (portamento.next) {
			     this.setArgs(output, args);
			}{
			// insert stop previous output here
                 this.prPlay(args);
			}
		*/
		this.release; // Release previous synth
		previousHasNoDurControl = nil;
		^this.prPlay(args);
	}

	release { | argDur |
		var output;
		if (chuck.isNil) {
			postf("% is trying to release with no chuck. This is abnormal\n");
			
			^nil;
		};
		output = chuck.output;
		if (argDur.notNil) {
			if (output isKindOf: Node) {
				if (output.isPlaying) {
					output.release(argDur);
					output = nil;
				}{
					output.onStart (this, { | notification |
						if (notification.listener.isPlaying) {
							notification.listener.release(argDur);
							output = nil;
						}
					})
				}
			}
		}{
			if ((previousHasNoDurControl ? hasNoDurControl) and: { output isKindOf: Node }) {
				output.release(chuck.args[\fadeTime].next);
			};
		}
	}

	prPlay { | args |
		^args use: source;
	}

	asChuckSource { ^this }
}

ChuckSynthSource : ChuckSource {
	examineSource {
		var desc;
		desc = SynthDescLib.at(source.asSymbol);
		if (desc.notNil) {
			hasNoDurControl = desc.controlNames.includes(\dur).not;
		}{
			hasNoDurControl = true;
		}
	}

	prPlay { | args |
		^this.prepareSynth(
			source.play(
				args [\target].next.asTarget,
				args [\out].next,
				args [\fadeTime].next,
				args [\addAction].next,
				args.getPairs
			)
		);
	}

	prepareSynth { | synth |
		^synth.onEnd(this, {
			if (chuck.output === synth) {
				chuck.output = nil;
			}
		})
	}
}

ChuckFuncSynthSource : ChuckSynthSource {
	var <defName;

	//	*new { | source, chuck |
	//	^super.new(source, chuck).makeSynthDef;
	// }

	examineSource {
		var def, desc;
		def = source.asSynthDef(
			fadeTime: chuck.args[\fadeTime],
			name: this.makeDefName
		).add; // add to SynthDescLib for use in ChuckSynthSource
		desc = def.desc;
		if (desc.controlNames includes: 'dur') {
			hasNoDurControl = false;
		}{
			hasNoDurControl = true;
		};
		this.moveToNullGroupIfNeeded(def);
		def.doSend(chuck.args[\target].server);
	}

	moveToNullGroupIfNeeded { | desc |
		"Effect Chuck's reading from 0 bus may bleed through.".postln;
		thisMethod.notImplemented;
		/*  TODO:
			Get the audio Inputs from SynthDesc.inputs, and if any of these are 
			audio rate, then set chuck.args[\target] to GroupLink.nullGroup
		*/
	}
	
	prPlay { | args |
		// play func only the first time.
		// thereafter, create synth from defName
		^this.prepareSynth(
			if (defName.isNil) {
				source.cplay(
					args [\target].next.asTarget,
					args [\out].next,
					args [\fadeTime].next,
					args [\addAction].next,
					args.getPairs,
					this.makeDefName;
				);
			}{
				defName.play(
					args [\target].next.asTarget,
					args [\out].next,
					args [\fadeTime].next,
					args [\addAction].next,
					args.getPairs
				)
			}
		)
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

