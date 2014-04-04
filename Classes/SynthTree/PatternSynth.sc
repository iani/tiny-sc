/*
Hold Group, Bus, Copy-Synth for playing a pattern's synths, inside a single SynthTree. 

TODO: Build interface for getting the instrument from a PatternInstrument. 

IZ Fri, Apr  4 2014, 12:49 EEST
*/

PatternSynth {

	var <synthTree, <numChannels, <bus, <synth, <group, <busIndex;

	*new { | synthTree, numChannels |
		^this.newCopyArgs(synthTree, numChannels ?? { ~numChans }).init;
	}

	init {
		bus = Bus.audio(synthTree.server, numChannels);
		busIndex = bus.index;
		group = Group(synthTree.group, \addToHead);
		this.makeCopySynth;
	}


	makeCopySynth {
		synth = { 
			Inp.ar(numChannels: numChannels).ladsrOut
		}.play(
			target: group,
			addAction: \addToTail,
			args: [in: busIndex, fadeIn: synthTree.getFadeTime, amp: 1]
		);
	}
	
	test {
		{ 
			LPF.ar(WhiteNoise.ar, (LFNoise0.kr(15).range(500, 5000))).ladsrOut
		}.play(args: [out: bus.index], target: group, addAction: \addToHead);
	}

	addSynth { | instrument = \default, args |
		^Synth(instrument, args ++ [out: busIndex], group, \addToHead)
	}

	addPattern { | pattern |
		synthTree.addNotifier(pattern, \value, { | synthEvent |
			var synth;
			synth = this.addSynth(\default, synthEvent.params);
			pattern.clock.sched(synthEvent.dur, { synth.release });
		});
	}

	isPlaying {
		^group.isPlaying;
	}

	release { | dur |
		synth release: dur;
	}

	set { | param, val | synth.set(param, val) }


}