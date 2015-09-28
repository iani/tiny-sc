
SynthPlayerLink {
	var <synthPlayer, <inputName;
	// Not yet implemented:
	var <xBus, <xBusLinkSynth; // optional branch out to other bus
	
	*new { | synthPlayerName, inputName = \in |
		^this.newCopyArgs (SynthPlayer (synthPlayerName.asSymbol), inputName.asSymbol);
	}
	synthPlayers { ^[synthPlayer] }

	setBussesAndGroups { | inBus, outBus, group | // used by MiniStereo
		synthPlayer.setInBus (inBus, inputName);
		synthPlayer setOutBus: outBus;
		synthPlayer setTarget: group;
		^group.getReaderGroup;
	}

	target { ^synthPlayer.target }

	inBus { ^synthPlayer inBus: inputName }

	outBus { ^synthPlayer.outBus }

	makeOutBus { ^ArBusLink () }
}