/*
Hold Group, Bus, Copy-Synth for playing a pattern's synths, inside a single SynthTree. 

TODO: Build interface for getting the instrument from a PatternInstrument. 

IZ Fri, Apr  4 2014, 12:49 EEST
*/

PatternSynth : Synth {

	var <synthTree, <bus, <busIndex;

	init { | argSynthTree, argBus |
		synthTree = argSynthTree;
		bus = argBus;
		busIndex = bus.index;
		this.onEnd(this, { | n |
			group.free;
			bus.free;
		})
	}

	test {
		^{
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

	moveAfter { | argNode |
		group.moveAfter(argNode);
		super.moveAfter(argNode);
	}

	moveBefore { | argNode |
		group.moveBefore(argNode);
	}

	moveToHead { | argNode |
		group.moveToHead(argNode);
	}

	moveToTail { | argNode |
		group.moveToTail(argNode);
	}
}