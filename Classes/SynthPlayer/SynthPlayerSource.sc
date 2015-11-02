/*
Thu, Jun  4 2015, 12:29 EEST
*/
SynthPlayerSource {
	var <source, <synthPlayer;
	var <hasNoDurControl = false, <previousHasNoDurControl = false;
	var hasAudioInputs = false;

	*new { | source, synthPlayer |
		^this.newCopyArgs( (source ?? { ["x", 0] }).asStream, synthPlayer).init
	}

	init {
		if (synthPlayer.notNil and: { synthPlayer.source.notNil}) { 
			previousHasNoDurControl = synthPlayer.source.hasNoDurControl;
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
		output = synthPlayer.output;
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
				output.release(synthPlayer.args[\fadeTime].next);
			};
		}
	}

	prPlay { | args |
		synthPlayer.output = args use: source;
	}

	asSynthPlayerSource { ^this }

	defLoaded {
		postf("% WARNING: A def was loaded for %, but I am unable to process it\n",
			this, synthPlayer);
	}
}

SynthPlayerSynthSource : SynthPlayerSource {
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
		synthPlayer.output = synth.onEnd(this, {
			if (synthPlayer.output === synth) { synthPlayer.output = nil; }
		})
	}
}

SynthPlayerFuncSynthSource : SynthPlayerSynthSource {
	classvar <linkRequest; // set by \synthPlayer.a calls inside source func
	var <defName;

	makeSource {
		var def, desc;
		def = source.asSynthDef(
			fadeTime: synthPlayer.args[\fadeTime],
			name: format("<%>", synthPlayer.name)
		);
		linkRequest !? {
			linkRequest &> synthPlayer;
			linkRequest = nil;
		};
		desc = def.asSynthDesc;
		SynthDescLib.default add: desc; // make def available to other SynthPlayers
		this processDesc: desc;
		// Send SynthDef to Server and notify when done:
		SynthDefLoader.add(synthPlayer, def, { this.defName = def.name; });
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

SynthPlayerPatternSource : SynthPlayerSynthSource {
	/* plays Event as EventPattern. See notes in file TODOs.org	*/
	//	var <player;
	//	var <bus, <group;

	*new { | event, synthPlayer |
		if (synthPlayer.source isKindOf: this) {
			^synthPlayer.source.addEvent(event);
		}{
			^this.newCopyArgs(event, synthPlayer).init;
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
			new target group inside SynthPlayer's target, new bus, and fade synth
			are created each time that the pattern plays with makeSynth. 
			1. Alloc new bus (happens immediately).
			2. Create group (is asynchronous - must use onStart to start synth after it)
			3. Create fade synth (asynchronous, use onStart to start pattern after it)
			4. Start playing EventPattern, 
		*/
		this.setSynth (EventPatternSynth(source, args));
	}
}
