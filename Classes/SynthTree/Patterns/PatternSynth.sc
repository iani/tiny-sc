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

	moveAfter { | argNode | group.moveAfter(argNode); }
	moveBefore { | argNode | group.moveBefore(argNode); }
	moveToHead { | argNode | group.moveToHead(argNode); }
	moveToTail { | argNode | group.moveToTail(argNode); }
}