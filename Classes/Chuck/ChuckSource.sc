/*
Thu, Jun  4 2015, 12:29 EEST
*/
ChuckSource {
	var <source, <chuck;
	var hasNoDurControl = true;
	var hasAudioInputs = false;

	*new { | source, chuck |
		^this.newCopyArgs( (source ?? { ["x", 0] }).asStream, chuck).init
	}

	init {}

	play { | output, args |
		/*
			if (portamento.next) {
			     this.setArgs(output, args);
			}{
			// insert stop previous output here
                 this.prPlay(args);
			}
		*/
		// Release previous synth
		if (hasNoDurControl and: { output isKindOf: Node }) {
			if (output.isPlaying) {
				output.release(args[\fadeTime].next)
			}{  
				output.onStart (this, { | notification |
 					if (notification.listener.isPlaying) {
						notification.listener.release(args[\fadeTime].next)
					}
				})
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
	init {
		var desc;
		desc = SynthDescLib.at(source.asSymbol);
		if (desc.notNil) {
			hasNoDurControl = desc.controlNames.includes(\dur).not;
		}{
			hasNoDurControl = true;
		}
	}

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

	//	*new { | source, chuck |
	//	^super.new(source, chuck).makeSynthDef;
	// }

	init {
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

