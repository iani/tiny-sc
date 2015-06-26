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

	prPlay {  | args | this.makeSynth(source, args) } // TODO: use args????

	makeSynth { | argDefName, args |
		var synth;
		// TODO 1: Replace this by args.play using default Event play mechanism
		// TODO 2: Incorporate bus mapping in synth creation by sending map with bundle
		synth = argDefName.play(
				args [\target].next.asTarget,
				args [\out].next,
				args [\fadeTime].next,
				args [\addAction].next,
				args.getPairs
		);
		this.setSynth (synth);
	}

	setSynth { | synth |
		chuck.output = synth.onEnd(this, {
			if (chuck.output === synth) { chuck.output = nil; }
		})
	}
}

ChuckFuncSynthSource : ChuckSynthSource {
	var <defName;

	makeSource {
		var def, desc;
		def = source.asSynthDef(
			fadeTime: chuck.args[\fadeTime],
			name: format("<%>", chuck.name)
		);
		desc = def.asSynthDesc;
		SynthDescLib.default add: desc; // make def available to other Chucks
		this processDesc: desc;
		// Send SynthDef to Server and notify when done:
		SynthDefLoader.add(chuck, def, { this.defName = def.name; });
	}

	defLoaded { | synthDefLoader | this.defName = synthDefLoader.synthDef.name; }
	
	defName_ { | argDefName |
		defName = argDefName.asString;
		this.changed(\defName);
	}
	
	prPlay { | args |
		if (defName.isNil) { // If SynthDef not yet loaded, wait for it to load.
			this.addNotifierOneShot(this, \defName, {
				this.makeSynth(defName, args)
			})
		}{
			this.makeSynth(defName, args);
		}
	}
}

ChuckPatternSource : ChuckSynthSource {
	/* plays Event as EventPattern. See notes in file TODOs.org	*/
	//	var <player;
	//	var <bus, <group;

	*new { | event, chuck |
		if (chuck.source isKindOf: this) {
			^chuck.source.addEvent(event);
		}{
			^this.newCopyArgs(event, chuck).init;
		}
	}

	makeSource {
		hasNoDurControl = true;
		source = EventPattern(source);
	}

	makeSynth { | source, args |
		/* source is an EventPattern.
			Args are indispensable for bus and target!
			Therefore copied to the event of EventPattern in source before playing.
			new target group inside Chuck's target, new bus, and fade synth
			are created each time that the pattern plays with makeSynth. 
			1. Alloc new bus (happens immediately).
			2. Create group (is asynchronous - must use onStart to start synth after it)
			3. Create fade synth (asynchronous, use onStart to start pattern after it)
			4. Start playing EventPattern, 
		*/
		this.setSynth (EventPatternSynth(source, args));
	}
}
