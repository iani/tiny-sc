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
		this.makeSource;
	}

	makeSource {}

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
		this.prPlay(args);
	}

	release { | argDur |
		var output;
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
		chuck.output = args use: source;
	}

	asChuckSource { ^this }

	defLoaded {
		postf("% WARNING: A def was loaded for %, but I am unable to process it\n",
			this, chuck);
	} 
}

ChuckSynthSource : ChuckSource {
	makeSource { this processDesc: SynthDescLib.at(source.asSymbol) }

	processDesc { | desc |
		if (desc.notNil) {
			hasNoDurControl = desc.controlNames.includes(\dur).not;
			
		}{
			hasNoDurControl = true;
		}
	}

	prPlay { | args | this makeSynth: source }

	makeSynth { | argDefName |
		var synth, args;
		// TODO 1: Replace this by args.play using default Event play mechanism
		// TODO 2: Incorporate bus mapping in synth creation by sending map with bundle
		args = chuck.args;
		synth = argDefName.play(
				args [\target].next.asTarget,
				args [\out].next,
				args [\fadeTime].next,
				args [\addAction].next,
				args.getPairs
		);
		chuck.output = synth.onEnd(this, {
			if (chuck.output === synth) { chuck.output = nil; }
		});
	}
}

ChuckFuncSynthSource : ChuckSynthSource {
	var <defName;

	makeSource {
		var def, desc;
		def = source.asSynthDef(
			fadeTime: chuck.args[\fadeTime],
			name: format("<%>", chuck.name)
		); // add to SynthDescLib for use in ChuckSynthSource
		desc = def.asSynthDesc;
		this processDesc: desc;
		SynthDescLib.default add: desc;
		SynthDefLoader.add(chuck, def, { this.defName = def.name; });
	}

	defLoaded { | synthDefLoader | this.defName = synthDefLoader.synthDef.name; }
	
	defName_ { | argDefName |
		defName = argDefName.asString;
		this.changed(\defName);
	}
	
	prPlay { | args |
		// If SynthDef not yet loaded, wait for it to load.
		if (defName.isNil) {
			this.addNotifierOneShot(this, \defName, {
				this.makeSynth(this.defName)
			})
		}{
			this.makeSynth(defName);
		}
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

