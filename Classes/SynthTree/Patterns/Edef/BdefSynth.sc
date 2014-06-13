/*
Hold Group, Bus, Copy-Synth for playing a pattern's synths, inside a single SynthTree. 
Cloned from earlier version PatternSynth.

IZ Fri, Wed, Apr 23 2014, 19:17 EEST
*/

BdefSynth : Synth {

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