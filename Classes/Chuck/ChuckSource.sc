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

ChuckPatternSource : ChuckSynthSource {
	/* plays Event as EventPattern
		The Event is stored in an EventPattern.
		The EventPattern is stored in source.
		The EventPattern is played through an EventStreamPlayer.
		The EventStreamPlayer is stored in player, and contains the EventStream that
		creates the Events.
		The ChuckPatternSource also creates a synth whose function 
		is to fade the output of the entire pattern in and out at beginning and end
		using the Chuck's fadeTime.	
		The Synth is used as Target for the EventStreamPlayer's synths, that 
		play with addAction "addBefore" relative to it.
		It is created inside the GroupLink group of the Chuck's args.
		It is stored as the Chuck's output.  When it stops,
		it also frees any synths created inside it by the pattern, and notifies the 
		pattern to stop. --- or maybe not it only relies on its fade env.
		- this simplifies things a bit.
		We still need a groupwhere everything is created in order to move everything around
		ar once when recobfiguring the synth tree.

		Since a new patternplayer may have started in the meanwhile, the stopping
		must be done by connecting the fadeSynth with Notification to the 
		EventStreamPlayer.
		The output of the EventPattern is directed to a private bus, that is the input of the 
		fadeSynth.
		group and bus are persistent.  Only the fade synth is started/sopped at each play 
		of the chuck.
	*/
	var <player;
	var <bus, <group;
	
}
